;; Usage: scm bin/init-config.scm <target>
;;
;; Writes a new config file at target from config.example.scm. Used by
;; deploy/install.sh; refuses to overwrite an existing file.

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scm fs)
        (scm module))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app config))

(define args (cdr (command-line)))
(when (null? args)
  (display "usage: scm bin/init-config.scm <target>\n")
  (exit 1))

(create-config! (string-append root "/config.example.scm") (car args))
(display (string-append "created " (car args) "\n"))
