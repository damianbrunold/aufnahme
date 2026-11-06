;; (app web) — routes, request handlers and the middleware stack.
;;
;; make-app returns the server-level handler (lambda (req) resp):
;; bin/server.scm serves it, and the tests call it directly.
;;
;; CSRF: the session cookie is SameSite=Lax, so browsers never send it with
;; a cross-site POST/PUT/DELETE; on top of that, every state-changing
;; request whose Origin header names another host is refused (403).
;; Forms follow POST -> 303 -> GET; the outcome travels as ?m=<code>.

(define-library (app web)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 18)
          (srfi 69)
          (scm crypto password)
          (scm datetime)
          (scm fs)
          (scm fs-find)
          (scm json simple)
          (scm log)
          (scm net http forms)
          (scm net http middleware)
          (scm net http multipart)
          (scm net http request)
          (scm net http response)
          (scm net http route)
          (scm net http static)
          (scm string)
          (app auth)
          (app media)
          (app projects)
          (app site)
          (app store)
          (app theme)
          (app timeline)
          (app users)
          (app views))
  (export app-jobs
          make-app)
  (begin

    (define cookie-name "aufnahme_session")

    ;; Screen capture needs these on top of the usual strict policy:
    ;; media-src/img-src blob: for local previews of the capture streams.
    (define csp
      (string-append
       "default-src 'self'; script-src 'self'; style-src 'self'; "
       "img-src 'self' data: blob:; media-src 'self' blob:; "
       "object-src 'none'; base-uri 'self'; form-action 'self'; "
       "frame-ancestors 'none'"))

    (define (setting settings key . default)
      (let ((p (assq key settings)))
        (cond (p (cdr p))
              ((pair? default) (car default))
              (else (error "make-app: missing setting" key)))))

    ;; --- small helpers ---------------------------------------------------

    (define (redirect path . headers)
      (make-http-response 303 (cons (cons "Location" (url path)) headers) ""))

    (define (penalty) (thread-sleep! 0.5))

    (define (query req) (url-query-params (http-request-url req)))
    (define (qref req name)
      (let ((p (assoc name (query req)))) (and p (cdr p))))

    (define (client-ip req)
      (let ((xff (http-request-header req "X-Forwarded-For")))
        (if (and xff (> (string-length xff) 0))
            ;; The last entry is the one our own proxy added.
            (let ((parts (string-split-char xff #\,)))
              (string-trim-both (list-ref parts (- (length parts) 1))))
            "local")))

    (define (string-trim-both s)
      (let* ((n (string-length s))
             (a (let l ((i 0))
                  (if (and (< i n) (char-whitespace? (string-ref s i)))
                      (l (+ i 1))
                      i)))
             (b (let l ((i n))
                  (if (and (> i a) (char-whitespace? (string-ref s (- i 1))))
                      (l (- i 1))
                      i))))
        (substring s a b)))

    ;; Where to go after signing in: a path of this site, never another host.
    (define (safe-next s)
      (and (string? s)
           (> (string-length s) 0)
           (< (string-length s) 2000)
           (char=? (string-ref s 0) #\/)
           (not (and (> (string-length s) 1)
                     (memv (string-ref s 1) '(#\/ #\\))))
           (let loop ((i 0))
             (or (= i (string-length s))
                 (and (char>? (string-ref s i) #\space)
                      (not (char=? (string-ref s i) #\\))
                      (loop (+ i 1)))))
           s))

    ;; Refuses state-changing requests started by another site.
    (define (with-origin-check handler)
      (lambda (req)
        (let ((m (http-request-method req))
              (origin (http-request-header req "Origin"))
              (host (http-request-header req "Host")))
          (if (and (member m '("POST" "PUT" "DELETE" "PATCH"))
                   origin
                   (not (and host
                             (let ((i (string-contains-from origin "://" 0)))
                               (string=?
                                (string-downcase
                                 (if i
                                     (substring origin
                                                (+ i 3)
                                                (string-length origin))
                                     origin))
                                (string-downcase host))))))
              (make-http-response 403
                                  '(("Content-Type" . "text/plain"))
                                  "cross-site request refused\n")
              (handler req)))))

    ;; At most `limit` failures per key within `window` seconds.
    (define (make-limiter limit window)
      (let ((table (make-hash-table equal?))
            (m (make-mutex)))
        (lambda (op key)
          (dynamic-wind
            (lambda () (mutex-lock! m))
            (lambda ()
              (let* ((now (now-seconds))
                     (recent (let loop
                                 ((ts (hash-table-ref/default table key '())))
                               (cond ((null? ts) '())
                                     ((< (- now (car ts)) window)
                                      (cons (car ts) (loop (cdr ts))))
                                     (else (loop (cdr ts)))))))
                (case op
                  ((blocked?)
                   (hash-table-set! table key recent)
                   (>= (length recent) limit))
                  ((fail!) (hash-table-set! table key (cons now recent)))
                  ((reset!) (hash-table-delete! table key)))))
            (lambda () (mutex-unlock! m))))))

    (define (file-extension name)
      (let loop ((i (- (string-length name) 1)))
        (cond ((< i 0) "")
              ((char=? (string-ref name i) #\.)
               (substring name (+ i 1) (string-length name)))
              (else (loop (- i 1))))))

    (define (sniff-image-kind bv)
      (cond ((and (>= (bytevector-length bv) 8)
                  (= (bytevector-u8-ref bv 0) #x89)
                  (= (bytevector-u8-ref bv 1) 80))
             "png")
            ((and (>= (bytevector-length bv) 12)
                  (equal? (bytevector-copy bv 0 4) (string->utf8 "RIFF"))
                  (equal? (bytevector-copy bv 8 12) (string->utf8 "WEBP")))
             "webp")
            ((let
                 ((s (guard (e (#t ""))
                       (utf8->string (bytevector-copy
                                      bv
                                      0
                                      (min 512 (bytevector-length bv)))))))
               (string-contains-from s "<svg" 0))
             "svg")
            (else #f)))

    (define app-jobs (make-parameter #f))

    (define (json-status status value)
      (make-http-response status
                          '(("Content-Type" .
                             "application/json; charset=utf-8")
                            ("Cache-Control" . "no-store"))
                          (json->string value)))

    (define (json-error status message)
      (json-status status `(("error" . ,message))))

    (define (parse-json-body req)
      (let ((body (http-request-body req)))
        (and (string? body)
             (guard (e (#t #f))
               (let ((j (json-parse body))) (and (list? j) j))))))

    (define (filter-alists xs)
      (let loop ((xs xs))
        (cond ((null? xs) '())
              ((list? (car xs)) (cons (car xs) (loop (cdr xs))))
              (else (loop (cdr xs))))))

    (define (free-bytes dir)
      ;; df reports 1K blocks.
      (let ((d (guard (e (#t '())) (df dir))))
        (and (pair? d)
             (let ((a (alist-ref (car d) 'available #f)))
               (and (number? a) (* a 1024))))))

    (define (clean-filename s)
      (let ((s (if (and (string? s) (> (string-length s) 0)) s "aufnahme")))
        (list->string (map (lambda (c)
                             (if (or (char<? c #\space)
                                     (memv
                                      c
                                      '(#\" #\/ #\\ #\: #\* #\? #\< #\> #\|)))
                                 #\_
                                 c))
                           (string->list
                            (if (> (string-length s) 120)
                                (substring s 0 120)
                                s))))))

    (define (today-string)
      ;; "06.11.2026" (UTC)
      (let ((iso (format-iso8601 (now-seconds))))
        (string-append (substring iso 8 10)
                       "."
                       (substring iso 5 7)
                       "."
                       (substring iso 0 4))))

    (define (write-bytevector-file path bv)
      (let ((tmp (string-append path ".tmp")))
        (let ((port (open-binary-output-file tmp)))
          (write-bytevector bv port)
          (close-port port))
        (move-file tmp path)))

    ;; --- the app ---------------------------------------------------------

    (define (make-app settings)
      "Syntax: (make-app settings)
Library: (app web)
Description: Builds the app's request handler. settings is an alist:
  url-prefix, static-dir, data-dir (absolute paths), session-secret,
  session-max-age (seconds), secure-cookies?, and optionally site-name,
  min-free-bytes, export-options (see export-arguments) and process? (#f:
  no background worker; processing runs inside the request)."
      (let* ((site (make-site (setting settings 'url-prefix)
                              (setting settings 'static-dir)))
             (prefix (site-prefix site))
             (data-dir (setting settings 'data-dir))
             (users-file (string-append data-dir "/users.scm"))
             (auth (make-auth cookie-name
                              (setting settings 'session-secret)
                              (setting settings 'session-max-age)
                              (cookie-path site)
                              (setting settings 'secure-cookies?)
                              users-file))
             (initial-name (setting settings 'site-name "Aufnahme"))
             (login-limit (make-limiter 10 600))
             (router (make-router))
             (min-free (setting settings
                                'min-free-bytes
                                (* 2 1024 1024 1024)))
             (export-options (setting settings 'export-options '()))
             (jobs (and (setting settings 'process? #t) (make-jobs)))
             (_ (begin
                  (ensure-dir (settings-dir data-dir))
                  (ensure-dir (string-append data-dir "/projects"))
                  (app-jobs jobs))))

        (define (route! method path handler)
          (router-add! router method (string-append prefix path) handler))

        (define (ctx user)
          `((user . ,user)
            (settings . ,(load-settings data-dir initial-name))
            (theme-v . ,(settings-version data-dir))))

        (define (msg-of req) (notice-text (qref req "m")))

        (define (not-found user)
          (make-http-response 404
                              '(("Content-Type" . "text/html; charset=utf-8"))
                              (not-found-page (ctx user))))

        (define (current-user req) (session-user auth req))

        ;; Runs handler with the user as a third argument.
        (define (with-role role handler)
          (lambda (req params)
            (let ((user (current-user req)))
              (cond ((has-role? user role) (handler req params user))
                    (user (make-http-response
                           403
                           '(("Content-Type" . "text/html; charset=utf-8"))
                           (not-found-page (ctx user))))
                    (else (redirect (string-append
                                     "/login?next="
                                     (url-path (http-request-url req)))))))))

        (define (signed-in handler) (with-role 'user handler))

        (define (own-project user params)
          ;; The project named in the URL, if user may see it.
          (let ((p (project-ref data-dir (params-ref params "pid"))))
            (and p
                 (or (equal? (alist-ref p 'owner) (user-name user))
                     (has-role? user 'admin))
                 p)))

        (define (with-project handler)
          (signed-in (lambda (req params user)
                       (let ((p (own-project user params)))
                         (if p
                             (handler req params user p)
                             (not-found user))))))

        (define (api-project handler)
          (lambda (req params)
            (let* ((user (current-user req))
                   (p (and (has-role? user 'user) (own-project user params))))
              (cond ((not (has-role? user 'user))
                     (json-error 401 "not signed in"))
                    ((not p) (json-error 404 "no such project"))
                    (else (handler req params user p))))))

        (define (take-url p t file)
          (url (string-append "/p/"
                              (alist-ref p 'id)
                              "/takes/"
                              (alist-ref t 'id)
                              "/"
                              file)))

        (define (edit-json p)
          (let* ((takes (let loop ((ts (project-takes data-dir p)))
                          (cond ((null? ts) '())
                                ((eq? (alist-ref (car ts) 'status) 'ready)
                                 (cons (car ts) (loop (cdr ts))))
                                (else (loop (cdr ts))))))
                 (cam (alist-ref p 'webcam '())))
            `(("takes"
               .
               ,(list->vector (map (lambda (t)
                                     (let ((c (alist-ref t 'cam)))
                                       `(("id" . ,(alist-ref t 'id))
                                         ("duration"
                                          .
                                          ,(alist-ref t 'duration 0))
                                         ("width" . ,(alist-ref t 'width 0))
                                         ("height" . ,(alist-ref t 'height 0))
                                         ("hasAudio"
                                          .
                                          ,(alist-ref t 'has-audio #f))
                                         ("screen"
                                          .
                                          ,(take-url
                                            p
                                            t
                                            (alist-ref (alist-ref t 'screen)
                                                       'file)))
                                         ("cam"
                                          .
                                          ,(if
                                            (and c (alist-ref c 'file))
                                            (take-url p t (alist-ref c 'file))
                                            'null))
                                         ("camOffset"
                                          .
                                          ,(alist-ref t 'cam-offset 0))
                                         ("strip"
                                          .
                                          ,(if (alist-ref t 'strip #f)
                                               (take-url p t "strip.jpg")
                                               'null))
                                         ("stripCount"
                                          .
                                          ,(alist-ref t 'strip 0))
                                         ("wave"
                                          .
                                          ,(if
                                            (and (alist-ref t 'has-audio #f)
                                                 (alist-ref t 'strip #f))
                                            (take-url p t "wave.png")
                                            'null)))))
                                   takes)))
              ("segments"
               .
               ,(list->vector (map (lambda (s)
                                     `(("take" . ,(alist-ref s 'take))
                                       ("start" . ,(alist-ref s 'start))
                                       ("end" . ,(alist-ref s 'end))))
                                   (clean-segments (alist-ref p 'segments #f)
                                                   (edit-takes data-dir p)))))
              ("webcam"
               .
               (("on" . ,(alist-ref cam 'on #t))
                ("corner"
                 .
                 ,(symbol->string (alist-ref cam 'corner 'bottom-right)))
                ("size" . ,(alist-ref cam 'size 0.22))))
              ("loudnorm" . ,(alist-ref p 'loudnorm #f)))))

        (define (process-take-later! pid tid)
          (take-update! data-dir pid tid
                        (lambda (t) (alist-set t 'status 'processing)))
          (enqueue-job! jobs
                        (string-append "take:" pid "/" tid)
                        (lambda () (process-take! data-dir pid tid))))

        (define (project-json p)
          `(("id" . ,(alist-ref p 'id))
            ("takes"
             .
             ,(list->vector (map (lambda (t)
                                   `(("id" . ,(alist-ref t 'id))
                                     ("status"
                                      .
                                      ,(symbol->string (alist-ref t 'status)))
                                     ("duration" . ,(alist-ref t 'duration 0))
                                     ("error"
                                      .
                                      ,(or (alist-ref t 'error) 'null))))
                                 (project-takes data-dir p))))
            ("export"
             .
             ,(let ((e (alist-ref p 'export '())))
                `(("status" . ,(symbol->string (alist-ref e 'status 'none)))
                  ("progress" . ,(alist-ref e 'progress 0))
                  ("error" . ,(or (alist-ref e 'error) 'null)))))))

        ;; --- static, theme, branding ---

        (route! "GET" "/static/*"
          (make-static-handler (setting settings 'static-dir)))

        (route! "GET" "/theme.css"
          (lambda (req params)
            (make-http-response 200
                                `(("Content-Type" . "text/css; charset=utf-8")
                                  ("Cache-Control"
                                   .
                                   ,(if (qref req "v")
                                        "public, max-age=31536000, immutable"
                                        "no-cache")))
                                (theme-css
                                 (load-settings data-dir initial-name)))))

        (route! "GET" "/site/:kind"
          (lambda (req params)
            (let* ((kind (params-ref params "kind"))
                   (s (load-settings data-dir initial-name))
                   (file (and (member kind '("logo" "favicon"))
                              (alist-ref s (string->symbol kind))))
                   (path (and (string? file)
                              (string-append (settings-dir data-dir)
                                             "/"
                                             file))))
              (if (and path (file-exists? path))
                  ;; An SVG opened directly runs no script (sandbox).
                  (make-http-file-response
                   200
                   `(("Content-Type"
                      .
                      ,(let ((e (assoc (file-extension file) logo-kinds)))
                         (if e (cdr e) "application/octet-stream")))
                     ("Cache-Control" . "public, max-age=86400")
                     ("Content-Security-Policy" .
                      "default-src 'none'; style-src 'unsafe-inline'; sandbox"))
                   path)
                  (make-http-response 404 '() "")))))

        ;; --- projects ---

        (route! "GET" "/"
          (signed-in (lambda (req params user)
                       (let* ((admin? (has-role? user 'admin))
                              (ps (let loop ((ps (project-list data-dir)))
                                    (cond
                                     ((null? ps) '())
                                     ((or
                                       admin?
                                       (equal? (alist-ref (car ps) 'owner)
                                               (user-name user)))
                                      (cons
                                       (cons
                                        (car ps)
                                        (project-takes data-dir (car ps)))
                                       (loop (cdr ps))))
                                     (else (loop (cdr ps)))))))
                         (html-response (home-page (ctx user) ps admin?))))))

        (route! "POST" "/projects"
          (signed-in (lambda (req params user)
                       (let
                           ((p (project-create!
                                data-dir
                                (user-name user)
                                (string-append "Aufnahme vom "
                                               (today-string)))))
                         (redirect (string-append "/p/"
                                                  (alist-ref p 'id)
                                                  "/record"))))))

        (route! "GET" "/p/:pid"
          (with-project
              (lambda (req params user p)
                (html-response (project-page (ctx user)
                                             p
                                             (project-takes data-dir p)
                                             (msg-of req))))))

        (route! "GET" "/p/:pid/record"
          (with-project
              (lambda (req params user p)
                (html-response (record-page (ctx user) p)))))

        (route! "POST" "/p/:pid/title"
          (with-project (lambda (req params user p)
                          (let
                              ((title (string-trim-both
                                       (form-ref (http-request-form req)
                                                 "title"
                                                 ""))))
                            (if (string=? title "")
                                (redirect
                                 (string-append "/p/"
                                                (alist-ref p 'id)
                                                "?m=no-title"))
                                (begin
                                  (project-update!
                                   data-dir
                                   (alist-ref p 'id)
                                   (lambda (p)
                                     (alist-set
                                      p
                                      'title
                                      (if (> (string-length title) 200)
                                          (substring title 0 200)
                                          title))))
                                  (redirect
                                   (string-append "/p/"
                                                  (alist-ref p 'id)
                                                  "?m=renamed"))))))))

        (route! "POST" "/p/:pid/delete"
          (with-project (lambda (req params user p)
                          (project-delete! data-dir (alist-ref p 'id))
                          (log-info "projects"
                                    (string-append (alist-ref p 'id)
                                                   " deleted by "
                                                   (user-name user)))
                          (redirect "/"))))

        (route! "POST" "/p/:pid/takes/:tid/delete"
          (with-project (lambda (req params user p)
                          (take-delete! data-dir
                                        (alist-ref p 'id)
                                        (params-ref params "tid"))
                          (redirect (string-append "/p/"
                                                   (alist-ref p 'id)
                                                   "?m=take-deleted")))))

        ;; A take whose browser never said "finished" (crash, closed tab):
        ;; process what arrived.
        (route! "POST" "/p/:pid/takes/:tid/finish"
          (with-project (lambda (req params user p)
                          (let* ((pid (alist-ref p 'id))
                                 (t (take-ref data-dir pid
                                              (params-ref params "tid"))))
                            (if (and t (eq? (alist-ref t 'status) 'recording))
                                (begin
                                  (process-take-later! pid (alist-ref t 'id))
                                  (redirect
                                   (string-append "/p/" pid
                                                  "?m=take-rescued")))
                                (redirect (string-append "/p/" pid)))))))

        (route! "GET" "/p/:pid/takes/:tid/:file"
          (with-project (lambda (req params user p)
                          (let* ((pid (alist-ref p 'id))
                                 (t (take-ref data-dir pid
                                              (params-ref params "tid")))
                                 (file (params-ref params "file"))
                                 (allowed
                                  (and t
                                       (list
                                        "thumb.jpg"
                                        "strip.jpg"
                                        "wave.png"
                                        (alist-ref (alist-ref t 'screen)
                                                   'file)
                                        (let ((c (alist-ref t 'cam)))
                                          (and c (alist-ref c 'file)))))))
                            (if (and allowed (member file allowed))
                                (file-response
                                 (take-file data-dir pid
                                            (alist-ref t 'id)
                                            file)
                                 `((request . ,req)
                                   (cache-control . "private, no-cache")))
                                (not-found user))))))

        (route! "POST" "/p/:pid/export"
          (with-project (lambda (req params user p)
                          (let ((pid (alist-ref p 'id)))
                            (if (null? (project-segments data-dir p))
                                (redirect
                                 (string-append "/p/" pid "?m=no-takes"))
                                (begin
                                  (project-update!
                                   data-dir
                                   pid
                                   (lambda (p)
                                     (alist-set
                                      p
                                      'export
                                      (alist-set
                                       (alist-set (alist-ref p 'export '())
                                                  'status
                                                  'queued)
                                       'progress
                                       0))))
                                  (enqueue-job!
                                   jobs
                                   (string-append "export:" pid)
                                   (lambda ()
                                     (export-project! data-dir pid
                                                      export-options)))
                                  (redirect
                                   (string-append "/p/" pid
                                                  "?m=export-queued"))))))))

        ;; --- editor ---

        (route! "GET" "/p/:pid/edit"
          (with-project (lambda (req params user p)
                          (let ((pid (alist-ref p 'id)))
                            (if (null? (edit-takes data-dir p))
                                (redirect
                                 (string-append "/p/" pid "?m=no-takes"))
                                (begin
                                  ;; Takes recorded before the editor existed (or whose
                                  ;; previews failed) get their film strip now.
                                  (for-each
                                   (lambda (t)
                                     (when
                                         (and
                                          (eq? (alist-ref t 'status) 'ready)
                                          (not (alist-ref t 'strip #f)))
                                       (enqueue-job!
                                        jobs
                                        (string-append "previews:" pid "/"
                                                       (alist-ref t 'id))
                                        (lambda ()
                                          (make-previews!
                                           data-dir
                                           pid
                                           (alist-ref t 'id))))))
                                   (project-takes data-dir p))
                                  (html-response
                                   (edit-page (ctx user) p))))))))

        (route! "GET" "/api/projects/:pid/edit"
          (api-project (lambda (req params user p)
                         (json-status 200 (edit-json p)))))

        (route! "PUT" "/api/projects/:pid/edit"
          (api-project (lambda (req params user p)
                         (let ((j (parse-json-body req)))
                           (if (not j)
                               (json-error 400 "JSON body required")
                               (let* ((pid (alist-ref p 'id))
                                      (takes (edit-takes data-dir p))
                                      (known
                                       (let ((k (json-ref j "knownTakes" #f)))
                                         (if (vector? k)
                                             (vector->list k)
                                             '())))
                                      (segs
                                       (let ((v (json-ref j "segments" #f)))
                                         (and
                                          (vector? v)
                                          (map
                                           (lambda (s)
                                             `((take .
                                                     ,(json-ref s "take" #f))
                                               (start
                                                .
                                                ,(json-ref s "start" #f))
                                               (end .
                                                    ,(json-ref s "end" #f))))
                                           (filter-alists
                                            (vector->list v))))))
                                      ;; Takes that became ready after the editor loaded
                                      ;; are kept, appended whole.
                                      (unseen
                                       (let loop ((ts takes))
                                         (cond
                                          ((null? ts) '())
                                          ((member (caar ts) known)
                                           (loop (cdr ts)))
                                          (else
                                           (cons
                                            `((take . ,(caar ts))
                                              (start . 0)
                                              (end . ,(cdar ts)))
                                            (loop (cdr ts)))))))
                                      (cam (json-ref j "webcam" '()))
                                      (corner
                                       (string->symbol
                                        (let
                                            ((c
                                              (json-ref cam "corner"
                                                        "bottom-right")))
                                          (if (string? c) c "bottom-right"))))
                                      (size (json-ref cam "size" 0.22)))
                                 (if (not segs)
                                     (json-error 400 "segments required")
                                     (begin
                                       (project-update!
                                        data-dir
                                        pid
                                        (lambda (p)
                                          (let*
                                              ((p
                                                (alist-set
                                                 p
                                                 'segments
                                                 (clean-segments
                                                  (append segs unseen)
                                                  takes)))
                                               (p
                                                (alist-set
                                                 p
                                                 'loudnorm
                                                 (eq?
                                                  (json-ref j "loudnorm" #f)
                                                  #t))))
                                            (alist-set
                                             p
                                             'webcam
                                             `((on
                                                .
                                                ,(not
                                                  (eq? (json-ref cam "on" #t)
                                                       #f)))
                                               (corner
                                                .
                                                ,(if
                                                  (memq
                                                   corner
                                                   '(top-left top-right
                                                     bottom-left
                                                     bottom-right))
                                                  corner
                                                  'bottom-right))
                                               (size
                                                .
                                                ,(if
                                                  (and (real? size)
                                                       (<= 0.05 size 0.5))
                                                  size
                                                  0.22))
                                               (margin . 24))))))
                                       (json-status
                                        200
                                        (edit-json
                                         (project-ref data-dir pid)))))))))))

        (route! "GET" "/p/:pid/export.mp4"
          (with-project
              (lambda (req params user p)
                (let
                    ((path (project-file data-dir
                                         (alist-ref p 'id)
                                         "export.mp4")))
                  (if (file-exists? path)
                      (file-response path
                                     `((request . ,req)
                                       (cache-control . "private, no-cache")
                                       ,@(if
                                          (qref req "download")
                                          `((filename
                                             .
                                             ,(string-append
                                               (clean-filename
                                                (alist-ref p 'title ""))
                                               ".mp4")))
                                          '())))
                      (not-found user))))))

        ;; --- recording API ---

        (route! "GET" "/api/projects/:pid"
          (api-project (lambda (req params user p)
                         (json-status 200 (project-json p)))))

        (route! "POST" "/api/projects/:pid/takes"
          (api-project (lambda (req params user p)
                         (let* ((j (parse-json-body req))
                                (smime (and j (json-ref j "screenMime" #f)))
                                (cmime (and j (json-ref j "camMime" #f)))
                                (free (free-bytes data-dir)))
                           (cond ((not (string? smime))
                                  (json-error 400 "screenMime required"))
                                 ((and free (< free min-free))
                                  (json-error 507
                                              "Nicht genügend Speicherplatz auf dem Server."))
                                 (else (let
                                           ((t
                                             (take-create!
                                              data-dir
                                              (alist-ref p 'id)
                                              smime
                                              (and (string? cmime) cmime))))
                                         (json-status
                                          201
                                          `(("id"
                                             .
                                             ,(alist-ref t 'id)))))))))))

        (route! "PUT" "/api/projects/:pid/takes/:tid/:track"
          (api-project (lambda (req params user p)
                         (let* ((track (params-ref params "track"))
                                (seq (string->number
                                      (or (qref req "seq") "")))
                                (bytes (http-request-body-bytes req)))
                           (cond ((not (member track '("screen" "cam")))
                                  (json-error 404 "no such track"))
                                 ((not (and (exact-integer? seq) (>= seq 0)))
                                  (json-error 400 "seq required"))
                                 ((not (bytevector? bytes))
                                  (json-error 400 "empty chunk"))
                                 (else (let
                                           ((r
                                             (take-append!
                                              data-dir
                                              (alist-ref p 'id)
                                              (params-ref params "tid")
                                              (string->symbol track)
                                              seq
                                              bytes)))
                                         (cond
                                          ((number? r)
                                           (json-status 200
                                                        `(("chunks" . ,r))))
                                          ((pair? r)
                                           (json-status
                                            409
                                            `(("error" . "out of order")
                                              ("expected" . ,(cdr r)))))
                                          (else
                                           (json-error 404
                                                       "no such take or not recording"))))))))))

        (route! "POST" "/api/projects/:pid/takes/:tid/finish"
          (api-project (lambda (req params user p)
                         (let* ((pid (alist-ref p 'id))
                                (tid (params-ref params "tid"))
                                (t (take-ref data-dir pid tid))
                                (j (or (parse-json-body req) '()))
                                (start (lambda (key)
                                         (let ((v (json-ref j key #f)))
                                           (and (real? v) v)))))
                           (cond ((not t) (json-error 404 "no such take"))
                                 ((not (eq? (alist-ref t 'status) 'recording))
                                  (json-status 200 '(("ok" . #t))))
                                 ((= 0
                                     (alist-ref (alist-ref t 'screen)
                                                'bytes
                                                0))
                                  (take-delete! data-dir pid tid)
                                  (json-error 400 "Die Aufnahme ist leer."))
                                 (else (take-update!
                                        data-dir
                                        pid
                                        tid
                                        (lambda (t)
                                          (let*
                                              ((t
                                                (alist-set
                                                 t
                                                 'screen
                                                 (alist-set
                                                  (alist-ref t 'screen)
                                                  'start
                                                  (start "screenStart")))))
                                            (if
                                             (alist-ref t 'cam)
                                             (alist-set
                                              t
                                              'cam
                                              (alist-set (alist-ref t 'cam)
                                                         'start
                                                         (start "camStart")))
                                             t))))
                                       (process-take-later! pid tid)
                                       (json-status
                                        200
                                        `(("ok" . #t)
                                          ("projectUrl"
                                           .
                                           ,(url
                                             (string-append "/p/"
                                                            pid)))))))))))

        ;; --- sign in ---

        (route! "GET" "/login"
          (lambda (req params)
            (if (current-user req)
                (redirect (or (safe-next (qref req "next")) "/"))
                (html-response (login-page (ctx #f)
                                           #f
                                           (safe-next (qref req "next")))))))

        (route! "POST" "/login"
          (lambda (req params)
            (let* ((form (http-request-form req))
                   (next (safe-next (form-ref form "next" "")))
                   (ip (client-ip req))
                   (user (and (not (login-limit 'blocked? ip))
                              (sign-in auth
                                       (form-ref form "username" "")
                                       (form-ref form "password" "")))))
              (if user
                  (begin
                    (login-limit 'reset! ip)
                    (redirect (or next "/") (start-session auth user)))
                  (begin
                    (login-limit 'fail! ip)
                    (penalty)
                    (html-response (login-page (ctx #f) #t next)))))))

        (route! "POST" "/logout"
          (lambda (req params) (redirect "/login" (end-session auth))))

        (route! "GET" "/profile"
          (signed-in (lambda (req params user)
                       (html-response (profile-page (ctx user)
                                                    (msg-of req))))))

        (route! "POST" "/profile/password"
          (signed-in (lambda (req params user)
                       (let* ((form (http-request-form req))
                              (new (form-ref form "new" ""))
                              (fail (lambda (text)
                                      (html-response
                                       (profile-page (ctx user)
                                                     (cons 'error text))))))
                         (cond ((not (sign-in auth
                                              (user-name user)
                                              (form-ref form "current" "")))
                                (penalty)
                                (fail "Das aktuelle Passwort ist falsch."))
                               ((< (string-length new) min-password-length)
                                (fail
                                 "Das neue Passwort muss mindestens 8 Zeichen lang sein."))
                               ((not (string=? new
                                               (form-ref form "confirm" "")))
                                (fail
                                 "Die neuen Passwörter stimmen nicht überein."))
                               (else (let
                                         ((user
                                           (change-password! auth user new)))
                                       (redirect
                                        "/profile?m=pw"
                                        (start-session auth user)))))))))

        ;; --- admin: users ---

        (route! "GET" "/admin/users"
          (with-role 'admin
            (lambda (req params user)
              (html-response (admin-users-page (ctx user)
                                               (load-users users-file)
                                               (msg-of req))))))

        (route! "POST" "/admin/users"
          (with-role 'admin
            (lambda (req params user)
              (let* ((form (http-request-form req))
                     (name (string-trim-both (form-ref form "username" "")))
                     (pw (form-ref form "password" ""))
                     (role (string->symbol (form-ref form "role" "user"))))
                (cond ((not (valid-username? name))
                       (redirect "/admin/users?m=bad-user"))
                      ((find-user users-file name)
                       (redirect "/admin/users?m=user-exists"))
                      ((< (string-length pw) min-password-length)
                       (redirect "/admin/users?m=short-pw"))
                      (else (save-user! users-file name
                                        (password-hash pw)
                                        (if (valid-role? role) role 'user))
                            (redirect "/admin/users?m=user-added")))))))

        (route! "POST" "/admin/users/:name"
          (with-role 'admin
            (lambda (req params user)
              (let* ((name (params-ref params "name"))
                     (form (http-request-form req))
                     (op (form-ref form "op" "")))
                (cond ((equal? name (user-name user))
                       (redirect "/admin/users?m=self"))
                      ((not (find-user users-file name)) (not-found user))
                      ((equal? op "role")
                       (let ((r (string->symbol (form-ref form "role" ""))))
                         (when (valid-role? r)
                           (update-user! users-file name 'role r))
                         (redirect "/admin/users?m=user-saved")))
                      ((member op '("disable" "enable"))
                       (update-user! users-file name
                                     'disabled
                                     (equal? op "disable"))
                       (redirect "/admin/users?m=user-saved"))
                      ((equal? op "password")
                       (let ((pw (form-ref form "password" "")))
                         (if (< (string-length pw) min-password-length)
                             (redirect "/admin/users?m=short-pw")
                             (begin
                               (update-user! users-file name
                                             'hash
                                             (password-hash pw))
                               (redirect "/admin/users?m=user-saved")))))
                      ((equal? op "delete")
                       (delete-user! users-file name)
                       (redirect "/admin/users?m=user-deleted"))
                      (else (redirect "/admin/users")))))))

        ;; --- admin: site settings ---

        (route! "GET" "/admin/site"
          (with-role 'admin
            (lambda (req params user)
              (let ((c (ctx user)))
                (html-response (admin-site-page
                                c
                                (msg-of req)
                                (contrast-warnings
                                 (alist-ref c 'settings))))))))

        (route! "POST" "/admin/site"
          (with-role 'admin
            (lambda (req params user)
              (let* ((form (http-request-form req))
                     (s (load-settings data-dir initial-name))
                     (colors (lambda (scheme)
                               (map (lambda (k)
                                      (cons
                                       (car k)
                                       (string-downcase
                                        (form-ref
                                         form
                                         (string-append
                                          (symbol->string scheme)
                                          "-"
                                          (symbol->string (car k)))
                                         ""))))
                                    color-keys)))
                     (light (colors 'light))
                     (dark (colors 'dark))
                     (mode (string->symbol
                            (form-ref form "default-mode" "auto")))
                     (name (string-trim-both (form-ref form "site-name" ""))))
                (if (not (let loop ((cs (append light dark)))
                           (or (null? cs)
                               (and (valid-color? (cdar cs))
                                    (loop (cdr cs))))))
                    (redirect "/admin/site?m=bad-color")
                    (begin
                      (save-settings! data-dir
                                      (let*
                                          ((s
                                            (alist-set
                                             s
                                             'site-name
                                             (if (string=? name "")
                                                 initial-name
                                                 name)))
                                           (s
                                            (alist-set
                                             s
                                             'tagline
                                             (string-trim-both
                                              (form-ref form "tagline" ""))))
                                           (s
                                            (alist-set
                                             s
                                             'default-mode
                                             (if
                                              (memq mode '(auto light dark))
                                              mode
                                              'auto)))
                                           (s (alist-set s 'light light)))
                                        (alist-set s 'dark dark)))
                      (redirect "/admin/site?m=site-saved")))))))

        (route! "POST" "/admin/site/reset"
          (with-role 'admin
            (lambda (req params user)
              (let ((s (load-settings data-dir initial-name)))
                ;; Keep the uploaded images; reset names and colours.
                (save-settings! data-dir
                                (alist-set
                                 (alist-set
                                  (alist-set default-settings
                                             'site-name
                                             initial-name)
                                  'logo
                                  (alist-ref s 'logo))
                                 'favicon
                                 (alist-ref s 'favicon)))
                (redirect "/admin/site?m=site-reset")))))

        (route! "POST" "/admin/site/:kind"
          (with-role 'admin
            (lambda (req params user)
              (let* ((kind (params-ref params "kind"))
                     (boundary (multipart-boundary
                                (or (http-request-header req "Content-Type")
                                    "")))
                     (parts (and boundary
                                 (guard (e (#t #f))
                                   (parse-multipart-bytes
                                    (http-request-body-bytes req)
                                    boundary))))
                     (file (and parts
                                (let loop ((ps parts))
                                  (cond
                                   ((null? ps) #f)
                                   ((equal? (part-ref (car ps) 'name) "file")
                                    (part-ref (car ps) 'body))
                                   (else (loop (cdr ps)))))))
                     (ext (and (bytevector? file)
                               (<= 1 (bytevector-length file) (* 1024 1024))
                               (sniff-image-kind file))))
                (cond ((not (member kind '("logo" "favicon")))
                       (not-found user))
                      ((not ext) (redirect "/admin/site?m=logo-bad"))
                      (else (let* ((s (load-settings data-dir initial-name))
                                   (old (alist-ref s (string->symbol kind)))
                                   (name (string-append kind "." ext)))
                              (write-bytevector-file
                               (string-append (settings-dir data-dir)
                                              "/"
                                              name)
                               file)
                              (when
                                  (and (string? old)
                                       (not (string=? old name)))
                                (rm (string-append (settings-dir data-dir)
                                                   "/"
                                                   old)
                                    'force))
                              (save-settings!
                               data-dir
                               (alist-set s (string->symbol kind) name))
                              (redirect "/admin/site?m=logo-saved"))))))))

        (route! "POST" "/admin/site/:kind/delete"
          (with-role 'admin
            (lambda (req params user)
              (let* ((kind (params-ref params "kind"))
                     (s (load-settings data-dir initial-name)))
                (if (not (member kind '("logo" "favicon")))
                    (not-found user)
                    (let ((old (alist-ref s (string->symbol kind))))
                      (when (string? old)
                        (rm (string-append (settings-dir data-dir) "/" old)
                            'force))
                      (save-settings! data-dir
                                      (alist-set s (string->symbol kind) #f))
                      (redirect "/admin/site?m=site-saved")))))))

        ;; Outside the prefix: the health probe talks to the app directly.
        (router-add! router "GET" "/healthz"
          (lambda (req params) (text-response "ok\n")))

        (with-access-log
            (with-security-headers
                (with-site site
                  (with-origin-check
                      (with-error-handler (router->handler router)
                        (lambda (req)
                          (html-response (error-page (ctx #f)))))))
              csp))))))
