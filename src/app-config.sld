;; (app config) — reads config.scm and creates it on the first run.
;;
;; The config file is a list of (define name expression) forms. It is read
;; as data, not loaded: each expression is evaluated on its own in a
;; (scheme base) environment, so (* 60 60 24 7) works but the file cannot
;; reach into the app. The result is an alist, read with config-ref.

(define-library (app config)
  (import (scheme base)
          (scheme eval)
          (scheme file)
          (scheme read)
          (scheme write)
          (scm crypto)
          (scm fs)
          (scm io)
          (scm string))
  (export config-ref
          config-secret
          create-config!
          load-config
          read-config
          resolve-path)
  (begin

    ;; config.example.scm carries this placeholder where a secret belongs;
    ;; create-config! replaces it with a fresh random key.
    (define secret-placeholder "REPLACE_WITH_BASE64_SECRET")

    (define (read-config path)
      "Syntax: (read-config path)
Library: (app config)
Description: Reads the (define name expression) forms in path and returns
  an alist of (name . value). A (load \"other.scm\") form reads other.scm
  in its place (the Debian package's /opt/aufnahme/app/config.scm points to
  /etc/aufnahme/config.scm this way). Any other form is an error."
      (let ((env (environment '(scheme base))))
        (call-with-input-file path
          (lambda (port)
            (let loop ((acc '()))
              (let ((form (read port)))
                (cond ((eof-object? form) (reverse acc))
                      ((and (pair? form)
                            (eq? (car form) 'define)
                            (pair? (cdr form))
                            (symbol? (cadr form))
                            (pair? (cddr form))
                            (null? (cdddr form)))
                       (loop (cons (cons (cadr form) (eval (caddr form) env))
                                   acc)))
                      ((and (pair? form)
                            (eq? (car form) 'load)
                            (pair? (cdr form))
                            (string? (cadr form))
                            (null? (cddr form)))
                       (loop (append (reverse (read-config (cadr form)))
                                     acc)))
                      (else (error "config: expected (define name value)"
                                   form)))))))))

    (define (create-config! example target)
      "Syntax: (create-config! example target)
Library: (app config)
Description: Writes target as a copy of example, with each secret
  placeholder replaced by a fresh base64 key, readable by the owner only.
  Refuses to overwrite an existing file."
      (when (file-exists? target) (error "config already exists" target))
      (let
          ((text (string-replace-all (read-file-string example)
                                     secret-placeholder
                                     (base64-encode (random-bytes 32)))))
        (call-with-output-file target (lambda (p) (write-string text p)))
        (chmod target "600")))

    (define (load-config root args)
      "Syntax: (load-config root args)
Library: (app config)
Description: Reads the config named by the first command-line argument in
  args, or else root/config.scm. On the first run, when root/config.scm
  does not exist yet, it is created from config.example.scm."
      (if (pair? args)
          (read-config (car args))
          (let ((path (string-append root "/config.scm")))
            (unless (file-exists? path)
              (create-config! (string-append root "/config.example.scm") path)
              (display (string-append "created " path "\n")
                       (current-error-port)))
            (read-config path))))

    (define (config-ref cfg name default)
      "Syntax: (config-ref cfg name default)
Library: (app config)
Description: The value of name in cfg, or default if the file has none."
      (let ((p (assq name cfg))) (if p (cdr p) default)))

    (define (config-secret cfg name)
      "Syntax: (config-secret cfg name)
Library: (app config)
Description: The secret stored under name: a base64 string, or a list of
  them (newest first) while rotating keys. Raises if it is missing or still
  the placeholder from config.example.scm."
      (let* ((s (config-ref cfg name #f))
             (keys (if (string? s) (list s) s)))
        (unless
            (and (pair? keys)
                 (let loop ((ks keys))
                   (or (null? ks)
                       (and (string? (car ks))
                            (not (string=? (car ks) secret-placeholder))
                            (loop (cdr ks))))))
          (error "config: set a secret (scm bin/gen-secret.scm prints one)"
                 name))
        s))

    (define (resolve-path root path)
      "Syntax: (resolve-path root path)
Library: (app config)
Description: path itself if absolute, else path relative to root."
      (if (and (> (string-length path) 0) (char=? (string-ref path 0) #\/))
          path
          (string-append root "/" path)))))
