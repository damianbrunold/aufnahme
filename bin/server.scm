;; Usage: scm bin/server.scm [config-path]
;;
;; Reads the config (config-path, or else config.scm, which the first run
;; creates from config.example.scm with a fresh secret) and serves the app.

(import (scheme base)
        (scheme process-context)
        (scm fs)
        (scm log)
        (scm module)
        (scm net http server))

;; Resolve the project root from this script's path so it runs from anywhere,
;; and make src/ importable.
(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app config)
        (app media)
        (app web))

(define cfg (load-config root (cdr (command-line))))
(define host (config-ref cfg 'http-host "127.0.0.1"))
(define port (config-ref cfg 'http-port 8073))
(define prefix (config-ref cfg 'url-prefix ""))
(define (path key default) (resolve-path root (config-ref cfg key default)))

(ffmpeg-program (config-ref cfg 'ffmpeg "ffmpeg"))
(ffprobe-program (config-ref cfg 'ffprobe "ffprobe"))

(define app
  (make-app `((url-prefix . ,prefix)
              (static-dir . ,(path 'static-dir "static"))
              (data-dir . ,(path 'data-dir "var"))
              (session-secret . ,(config-secret cfg 'session-secret))
              (session-max-age .
                               ,(config-ref cfg
                                            'session-max-age
                                            (* 60 60 24 14)))
              (secure-cookies? . ,(config-ref cfg 'secure-cookies? #t))
              (site-name . ,(config-ref cfg 'site-name "Aufnahme"))
              (min-free-bytes .
                              ,(config-ref cfg
                                           'min-free-bytes
                                           (* 2 1024 1024 1024)))
              (export-options .
                              ((preset .
                                       ,(config-ref cfg
                                                    'export-preset
                                                    "veryfast"))
                               (crf . ,(config-ref cfg 'export-crf 23))
                               (max-width
                                .
                                ,(config-ref cfg 'export-max-width 1920)))))))

(log-info "app"
          (string-append "aufnahme listening on http://" host ":"
                         (number->string port)
                         prefix
                         "/"))
(serve-forever port app
               (config-ref cfg 'http-max-threads 32)
               host
               120000
               (* 8 1024 1024))
