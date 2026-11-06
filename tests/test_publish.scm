;; Integration test: publishing an export to a real filme server.
;;
;; Starts ../filme (the sibling checkout) on a spare port with its own
;; temporary data folder, creates an editor and an API token there, then
;; records, exports and publishes a project here. Needs ffmpeg; skipped
;; (with a note) when ../filme is missing.
;; Run: scm tests/test_publish.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (srfi 18)
        (scm crypto)
        (scm crypto password)
        (scm fs)
        (scm json simple)
        (scm log)
        (scm module)
        (scm net http client)
        (scm net http request)
        (scm net http response)
        (scm random access)
        (scm string)
        (scm system)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app projects)
        (app store)
        (app users)
        (app web))

(define filme-root (string-append (directory-name root) "/filme"))
(unless (file-exists? (string-append filme-root "/bin/server.scm"))
  (display "test_publish: ../filme not found, skipped\n")
  (exit 0))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme publish")

(define tmp (mktempdir '(prefix . "aufnahme-publish")))
(define port 8199)
(define filme-url (string-append "http://127.0.0.1:" (number->string port)))

;; --- a filme server -----------------------------------------------------

(define filme-config (string-append tmp "/filme-config.scm"))
(call-with-output-file filme-config
  (lambda (p)
    (for-each (lambda (line)
                (write-string line p)
                (newline p))
              (list "(define http-host \"127.0.0.1\")"
                    (string-append "(define http-port "
                                   (number->string port)
                                   ")")
                    (string-append "(define session-secret \""
                                   (base64-encode (random-bytes 32))
                                   "\")")
                    "(define secure-cookies? #f)"
                    (string-append "(define data-dir \"" tmp "/filme-data\")")
                    "(define min-free-bytes 0)"))))

(define (filme-tool . args)
  (run-program/capture (append (list "scm"
                                     (string-append filme-root
                                                    "/bin/user.scm")
                                     "--config"
                                     filme-config)
                               args)
                       '((stdin "editor-pass\neditor-pass\n"))))

(test-assert "filme user" (filme-tool "add" "ed" "editor"))
(define filme
  (start-program (list "scm"
                       (string-append filme-root "/bin/server.scm")
                       filme-config)
                 `((log-file ,(string-append tmp "/filme.log")))))
(process-kill-on-exit filme)
(let wait ((n 0))
  (unless
      (or (> n 100)
          (guard (e (#t #f))
            (= 200
               (http-response-status (http-get
                                      (string-append filme-url
                                                     "/healthz"))))))
    (thread-sleep! 0.2)
    (wait (+ n 1))))

(define (body-of r)
  (let ((b (http-response-body r)))
    (if (bytevector? b) (utf8->string b) (or b ""))))

;; Sign in to filme and create a token, as a user would on the profile page.
(define filme-token
  (let* ((r (http-send (make-http-request
                        "POST"
                        (string-append filme-url "/login")
                        '(("Content-Type" .
                           "application/x-www-form-urlencoded"))
                        "username=ed&password=editor-pass"
                        30
                        #f)))
         (cookie (car (string-split-char (http-response-header r "Set-Cookie")
                                         #\;)))
         (page (body-of (http-send (make-http-request
                                    "POST"
                                    (string-append filme-url
                                                   "/profile/tokens")
                                    (list
                                     (cons "Cookie" cookie)
                                     '("Content-Type" .
                                       "application/x-www-form-urlencoded"))
                                    "label=Aufnahme"
                                    30))))
         (i (string-contains-from page "value=\"fltk_" 0)))
    (and i (substring page (+ i 7) (+ i 7 60)))))
(test-assert "filme token" (string? filme-token))

;; --- aufnahme --------------------------------------------------------------

(parameterize ((password-pbkdf2-iterations 1000))
  (save-user! (string-append tmp "/users.scm")
              "uli"
              (password-hash "uli password")
              'user))

(define app
  (make-app `((url-prefix . "")
              (static-dir . ,(string-append root "/static"))
              (data-dir . ,tmp)
              (session-secret . ,(base64-encode (random-bytes 32)))
              (session-max-age . 3600)
              (secure-cookies? . #f)
              (process? . #f)
              (min-free-bytes . 0)
              (filme-url . ,(string-append filme-url "/"))
              (export-options . ((preset . "ultrafast"))))))

(define (send method url cookie body . headers)
  (parameterize ((log-port (open-output-string))
                 (password-pbkdf2-iterations 1000))
    (app (make-http-request method url
                            (append (if cookie
                                        (list (cons "Cookie" cookie))
                                        '())
                                    headers)
                            body))))

(define form '("Content-Type" . "application/x-www-form-urlencoded"))
(define json '("Content-Type" . "application/json"))

(define cookie
  (car (string-split-char (http-response-header
                           (send "POST" "/login" #f
                                 "username=uli&password=uli+password" form)
                           "Set-Cookie")
                          #\;)))

(define pid
  (substring (http-response-header (send "POST" "/projects" cookie "" form)
                                   "Location")
             3
             14))

;; One take, uploaded in a single chunk, then exported.
(define clip (string-append tmp "/clip.webm"))
(run-program (list "ffmpeg" "-v" "error" "-y" "-f" "lavfi" "-i"
                   "testsrc2=duration=3:size=640x360:rate=25" "-f" "lavfi"
                   "-i" "sine=duration=3" "-c:v" "libvpx" "-c:a" "libopus"
                   clip))
(let* ((r (send "POST"
                (string-append "/api/projects/" pid "/takes")
                cookie
                "{\"screenMime\":\"video/webm\",\"camMime\":null}"
                json))
       (tid (substring (body-of r) 7 18)))
  (send "PUT"
        (string-append "/api/projects/" pid "/takes/" tid "/screen?seq=0")
        cookie
        (call-with-random-access-file clip
          'read
          (lambda (f) (random-access-file-read f 0 (file-size clip)))))
  (send "POST"
        (string-append "/api/projects/" pid "/takes/" tid "/finish")
        cookie
        "{}"
        json))
(send "POST" (string-append "/p/" pid "/export") cookie "" form)
(test-equal 'ready
            (alist-ref (alist-ref (project-ref tmp pid) 'export) 'status))

(test-group "the filme token on the profile"
  (test-assert (string-contains-from (body-of
                                      (send "GET" "/profile" cookie #f))
                                     "Verbindung zu filme"
                                     0))
  (test-equal "/profile?m=filme-bad-token"
              (http-response-header (send "POST" "/profile/filme" cookie
                                          "token=fltk_wrong" form)
                                    "Location"))
  (test-equal #f
              (alist-ref (find-user (string-append tmp "/users.scm") "uli")
                         'filme-token
                         #f))
  (test-equal "/profile?m=filme-saved"
              (http-response-header (send "POST" "/profile/filme" cookie
                                          (string-append "token=" filme-token)
                                          form)
                                    "Location"))
  (test-equal filme-token
              (alist-ref (find-user (string-append tmp "/users.scm") "uli")
                         'filme-token)))

(test-group "publish"
  (test-assert "the form is offered"
               (string-contains-from (body-of
                                      (send "GET"
                                            (string-append "/p/" pid)
                                            cookie
                                            #f))
                                     "In filme veröffentlichen"
                                     0))
  (test-equal (string-append "/p/" pid "?m=publish-queued")
              (http-response-header (send
                                     "POST"
                                     (string-append "/p/" pid "/publish")
                                     cookie
                                     "title=Git+Kurs&description=Teil+1&tags=git%2C+intro"
                                     form)
                                    "Location"))
  (let ((pub (alist-ref (project-ref tmp pid) 'publish)))
    (test-equal 'done (alist-ref pub 'status))
    (test-assert (string? (alist-ref pub 'video)))
    (test-assert (string-contains-from (alist-ref pub 'url)
                                       (string-append filme-url "/v/")
                                       0))
    ;; filme knows the video now, with our title, as a draft.
    (let* ((r (http-send (make-http-request
                          "GET"
                          (string-append filme-url "/api/videos/"
                                         (alist-ref pub 'video))
                          (list (cons "Authorization"
                                      (string-append "Bearer " filme-token)))
                          #f
                          30)))
           (j (json-parse (body-of r))))
      (test-equal 200 (http-response-status r))
      (test-assert (member (json-ref j "status" "") '("processing" "ready"))))
    (let
        ((meta (read-datum (string-append tmp "/filme-data/videos/"
                                          (alist-ref pub 'video)
                                          "/meta.scm")
                           '())))
      (test-equal "Git Kurs" (alist-ref meta 'title))
      (test-equal "Teil 1" (alist-ref meta 'description))
      (test-equal '("git" "intro") (alist-ref meta 'tags))
      (test-equal 'draft (alist-ref meta 'visibility))
      (test-equal "ed" (alist-ref meta 'created-by))))
  (test-assert "the result is shown"
               (string-contains-from (body-of
                                      (send "GET"
                                            (string-append "/p/" pid)
                                            cookie
                                            #f))
                                     "In filme öffnen"
                                     0)))

(test-group "a revoked token fails cleanly"
  (update-user! (string-append tmp "/users.scm")
                "uli"
                'filme-token
                "fltk_AAAAAAAAAAA_revoked")
  (send "POST"
        (string-append "/p/" pid "/publish")
        cookie
        "title=Nochmal"
        form)
  (let ((pub (alist-ref (project-ref tmp pid) 'publish)))
    (test-equal 'failed (alist-ref pub 'status))
    (test-assert (string-contains-from (alist-ref pub 'error) "Token" 0))))

(process-kill filme)
(process-wait filme 5000)
(rm tmp 'recursive 'force)
(test-end "aufnahme publish")
