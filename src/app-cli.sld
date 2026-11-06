;; (app cli) — shared plumbing for the bin/ scripts: the optional
;; "--config path" argument, the data directory and error exits.

(define-library (app cli)
  (import (scheme base)
          (scheme process-context)
          (scheme write)
          (app config))
  (export cli-config
          cli-data-dir
          cli-fail)
  (begin

    (define (cli-fail msg)
      "Syntax: (cli-fail msg)
Library: (app cli)
Description: Prints msg to stderr and exits with status 1."
      (display msg (current-error-port))
      (newline (current-error-port))
      (exit 1))

    (define (cli-config root args)
      "Syntax: (cli-config root args)
Library: (app cli)
Description: Reads the config named by a leading --config path (else
  AUFNAHME_CONFIG, else root/config.scm),
  and returns (cfg . remaining-args)."
      (let* ((explicit (and (pair? args)
                            (string=? (car args) "--config")
                            (pair? (cdr args))
                            (cadr args)))
             (rest (if explicit (cddr args) args))
             (env (get-environment-variable "AUFNAHME_CONFIG"))
             (cfg (load-config root
                               (cond (explicit (list explicit))
                                     ((and env (> (string-length env) 0))
                                      (list env))
                                     (else '())))))
        (cons cfg rest)))

    (define (cli-data-dir root cfg)
      "Syntax: (cli-data-dir root cfg)
Library: (app cli)
Description: The absolute data directory of cfg."
      (resolve-path root (config-ref cfg 'data-dir "var")))))
