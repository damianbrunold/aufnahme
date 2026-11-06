;; (app site) — the URL prefix and asset URLs.
;;
;; The app can be mounted under a path ("/team"), with the reverse proxy
;; passing paths through unchanged. Every route, link and redirect goes
;; through url, and static files through asset. make-app in (app web) makes
;; its site current for each request with with-site.

(define-library (app site)
  (import (scheme base)
          (scm net http static))
  (export asset
          cookie-path
          make-site
          site-prefix
          url
          with-site)
  (begin

    (define-record-type site
      (%make-site prefix static-url)
      site?
      (prefix site-prefix)
      (static-url site-static-url))

    (define current-site (make-parameter #f))

    ;; "" or "/" -> ""; "team/" -> "/team"
    (define (normalize-prefix p)
      (let* ((s (if (and (> (string-length p) 0)
                         (char=? (string-ref p 0) #\/))
                    p
                    (string-append "/" p)))
             (n (string-length s)))
        (if (char=? (string-ref s (- n 1)) #\/) (substring s 0 (- n 1)) s)))

    (define (make-site prefix static-dir)
      "Syntax: (make-site prefix static-dir)
Library: (app site)
Description: A site mounted at prefix (\"\" for the host root) that serves
  static-dir under prefix/static."
      (let ((p (normalize-prefix prefix)))
        (%make-site p
                    (make-static-url static-dir
                                     (string-append p "/static")))))

    (define (with-site site handler)
      "Syntax: (with-site site handler)
Library: (app site)
Description: Wraps handler so url and asset refer to site while it runs."
      (lambda (req) (parameterize ((current-site site)) (handler req))))

    (define (url path)
      "Syntax: (url path)
Library: (app site)
Description: path under the current site's prefix: (url \"/login\")."
      (string-append (site-prefix (current-site)) path))

    (define (asset rel)
      "Syntax: (asset rel)
Library: (app site)
Description: The URL of static/rel with a ?v=<mtime> cache buster."
      ((site-static-url (current-site)) rel))

    (define (cookie-path site)
      "Syntax: (cookie-path site)
Library: (app site)
Description: The cookie Path for site: \"/\", or \"/prefix/\" so that the
  session cookie stays with this app when several share a host."
      (string-append (site-prefix site) "/"))))
