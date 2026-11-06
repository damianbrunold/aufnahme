;; Tests for the web app: sign-in, roles, home page, admin pages, CSRF
;; origin check, theming and the URL prefix. Requests go straight to the
;; handler from make-app (no server, no socket).
;; Run: scm tests/test_pure_web.scm

(import (scheme base)
        (scheme process-context)
        (scm crypto)
        (scm crypto password)
        (scm fs)
        (scm log)
        (scm module)
        (scm net http request)
        (scm net http response)
        (scm string)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app users)
        (app web))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme web")

(define tmp (mktempdir '(prefix . "aufnahme-web")))
(define users-file (string-append tmp "/users.scm"))
(define fast-iterations 1000)

(define (app-with prefix)
  (make-app `((url-prefix . ,prefix)
              (static-dir . ,(string-append root "/static"))
              (data-dir . ,tmp)
              (session-secret . ,(base64-encode (random-bytes 32)))
              (session-max-age . 3600)
              (secure-cookies? . #f))))

;; more: optional body (form string) and a list of extra header pairs.
(define (send app method url cookie . more)
  (let ((body (if (pair? more) (car more) #f))
        (headers (if (and (pair? more) (pair? (cdr more))) (cadr more) '())))
    (parameterize ((log-port (open-output-string))
                   (password-pbkdf2-iterations fast-iterations))
      (app (make-http-request method url
                              (append (if cookie
                                          (list (cons "Cookie" cookie))
                                          '())
                                      (if
                                       (string? body)
                                       '(("Content-Type" .
                                          "application/x-www-form-urlencoded"))
                                       '())
                                      headers)
                              body)))))

(define (status r) (http-response-status r))
(define (location r) (http-response-header r "Location"))
(define (body r)
  (let ((b (http-response-body r)))
    (if (bytevector? b) (utf8->string b) (or b ""))))
(define (has? s part) (and (string? s) (string-contains-from s part 0) #t))
(define (cookie-of r)
  (let ((h (http-response-header r "Set-Cookie")))
    (and h (car (string-split-char h #\;)))))

(parameterize ((password-pbkdf2-iterations fast-iterations))
  (save-user! users-file "uli" (password-hash "uli password") 'user)
  (save-user! users-file "ada" (password-hash "ada password") 'admin))

(define app (app-with ""))

(define (login name pw)
  (cookie-of (send app "POST" "/login" #f
                   (string-append "username=" name "&password=" pw))))

(define uli (login "uli" "uli+password"))
(define ada (login "ada" "ada+password"))

(test-group "signed out"
  (test-equal "/login?next=/" (location (send app "GET" "/" #f)))
  (test-equal 200 (status (send app "GET" "/login" #f)))
  (test-equal "ok\n" (body (send app "GET" "/healthz" #f)))
  (test-equal 200 (status (send app "GET" "/theme.css" #f))))

(test-group "sign in"
  (test-assert uli)
  (let ((r (send app "POST" "/login" #f "username=uli&password=wrong")))
    (test-equal 200 (status r))
    (test-assert (has? (body r) "falsch"))
    (test-equal #f (http-response-header r "Set-Cookie")))
  (let
      ((r (send app "POST" "/login" #f
                "username=uli&password=uli+password&next=/profile")))
    (test-equal "/profile" (location r))
    (test-assert "SameSite=Lax"
                 (has? (http-response-header r "Set-Cookie") "SameSite=Lax"))
    (test-assert "HttpOnly"
                 (has? (http-response-header r "Set-Cookie") "HttpOnly")))
  (test-equal "foreign next ignored" "/"
              (location (send app "POST" "/login" #f
                              "username=uli&password=uli+password&next=//evil.example")))
  (test-equal 303 (status (send app "GET" "/" "aufnahme_session=forged"))))

(test-group "home"
  (let ((r (send app "GET" "/" uli)))
    (test-equal 200 (status r))
    (test-assert "browser check"
                 (has? (body r) "Kann dieser Browser aufnehmen?"))
    (test-assert (has? (body r) "js/support.js"))
    (test-assert "no admin links for users"
                 (not (has? (body r) "/admin/users")))
    (test-assert "no inline script" (not (has? (body r) "<script>")))
    (test-assert "no style attribute" (not (has? (body r) " style="))))
  (test-assert (has? (body (send app "GET" "/" ada)) "/admin/users"))
  (let
      ((csp (http-response-header (send app "GET" "/" uli)
                                  "Content-Security-Policy")))
    (test-assert (has? csp "media-src 'self' blob:"))
    (test-assert (has? csp "script-src 'self'"))))

(test-group "roles"
  (test-equal 403 (status (send app "GET" "/admin/users" uli)))
  (test-equal 403 (status (send app "GET" "/admin/site" uli)))
  (test-equal 200 (status (send app "GET" "/admin/users" ada)))
  (test-equal 200 (status (send app "GET" "/admin/site" ada)))
  (test-equal 200 (status (send app "GET" "/profile" uli))))

(test-group "change password"
  (let* ((r (send app "POST" "/profile/password" uli
                  "current=uli+password&new=newpassword&confirm=newpassword"))
         (fresh (cookie-of r)))
    (test-equal "/profile?m=pw" (location r))
    (test-equal "old session ended" 303 (status (send app "GET" "/" uli)))
    (test-equal 200 (status (send app "GET" "/" fresh)))
    (set! uli fresh))
  (test-assert (has? (body (send app "POST" "/profile/password" uli
                                 "current=wrong&new=newpassword2&confirm=newpassword2"))
                     "falsch")))

(test-group "admin users"
  (test-equal "/admin/users?m=user-added"
              (location (send app "POST" "/admin/users" ada
                              "username=neu&password=neupasswort&role=admin")))
  (test-equal 'admin (user-role (find-user users-file "neu")))
  (test-equal "/admin/users?m=user-exists"
              (location (send app "POST" "/admin/users" ada
                              "username=neu&password=neupasswort")))
  (test-equal "/admin/users?m=bad-user"
              (location (send app "POST" "/admin/users" ada
                              "username=a+b&password=neupasswort")))
  (test-equal "/admin/users?m=self"
              (location (send app "POST" "/admin/users/ada" ada "op=delete")))
  (send app "POST" "/admin/users/neu" ada "op=role&role=user")
  (test-equal 'user (user-role (find-user users-file "neu")))
  (send app "POST" "/admin/users/uli" ada "op=disable")
  (test-equal "disabled user is signed out" 303
              (status (send app "GET" "/" uli)))
  (send app "POST" "/admin/users/uli" ada "op=enable")
  (test-equal 200 (status (send app "GET" "/" uli)))
  (send app "POST" "/admin/users/neu" ada "op=delete")
  (test-equal #f (find-user users-file "neu")))

(test-group "cross-site requests are refused"
  (test-equal 403
              (status (send app "POST" "/logout" uli ""
                            '(("Origin" . "https://evil.example")
                              ("Host" . "rec.local")))))
  (test-equal 303
              (status (send app "POST" "/logout" uli ""
                            '(("Origin" . "http://rec.local")
                              ("Host" . "rec.local"))))))

(test-group "site settings"
  (test-equal "/admin/site?m=bad-color"
              (location (send app "POST" "/admin/site" ada
                              "site-name=X&light-bg=red")))
  (let
      ((form (string-append
              "site-name=Studio&tagline=Intern&default-mode=dark"
              (apply string-append
                     (map (lambda (k)
                            (string-append "&light-" k "=%23ffffff&dark-" k
                                           "=%23000000"))
                          '("bg" "surface" "fg" "muted" "accent" "on-accent"
                            "border"))))))
    (test-equal "/admin/site?m=site-saved"
                (location (send app "POST" "/admin/site" ada form))))
  (let ((page (body (send app "GET" "/login" #f))))
    (test-assert (has? page "Studio"))
    (test-assert (has? page "data-theme-default=\"dark\""))
    (test-assert (has? page "Intern")))
  (test-assert (has? (body (send app "GET" "/theme.css" #f))
                     "color-scheme: dark"))
  (test-equal "/admin/site?m=site-reset"
              (location (send app "POST" "/admin/site/reset" ada "")))
  (test-assert "reset to the configured name"
               (has? (body (send app "GET" "/login" #f)) "Aufnahme")))

(test-group "url prefix"
  (let* ((app (app-with "/rec"))
         (r (send app "POST" "/rec/login" #f
                  "username=ada&password=ada+password")))
    (test-equal "/rec/" (location r))
    (test-assert (has? (http-response-header r "Set-Cookie") "Path=/rec/"))
    (test-assert (has? (body (send app "GET" "/rec/login" #f))
                       "href=\"/rec/static/css/app.css?v="))
    (test-equal 404 (status (send app "GET" "/login" #f)))
    (test-equal "ok\n" (body (send app "GET" "/healthz" #f)))))

(rm tmp 'recursive 'force)
(test-end "aufnahme web")
