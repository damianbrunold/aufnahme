;; aufnahme configuration: a list of (define name value) forms, read as
;; data by src/app-config.sld. bin/server.scm reads config.scm; the first
;; run creates it from this file, with a fresh session-secret.

;; Screen capture only works on HTTPS or on http://localhost. Bind
;; loopback: either a TLS reverse proxy fronts the app (central server,
;; see deploy/nginx.conf.example), or the browser on this machine opens
;; http://localhost:8073 directly (local recording PC).
(define http-host "127.0.0.1")
(define http-port 8073)
(define http-max-threads 32)

;; Path the app is mounted under, e.g. "/aufnahme"; "" for the host root.
(define url-prefix "")

;; Signs the session cookies (base64 of 32 random bytes). To rotate it,
;; list the new key first: ("new" "old"). bin/gen-secret.scm prints one.
(define session-secret "REPLACE_WITH_BASE64_SECRET")
(define session-max-age (* 60 60 24 14)) ; 14 days

;; Cookies are sent over HTTPS only when #t. Keep #f for http://localhost;
;; the Debian package sets #t (set it back to #f for a local install).
(define secure-cookies? #f)

;; The data folder: projects, recordings, exports, users, branding.
;; Relative paths are relative to the app.
(define data-dir "var")
(define static-dir "static")

;; The site name until an admin changes it under "Darstellung".
(define site-name "Aufnahme")

;; Recording is refused when less than this much disk space would remain.
(define min-free-bytes (* 2 1024 1024 1024))

;; Programs used for processing (must be on the PATH, or give full paths).
(define ffmpeg "ffmpeg")
(define ffprobe "ffprobe")
