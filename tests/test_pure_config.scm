;; Tests for (app config). Run: scm tests/test_pure_config.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm fs)
        (scm module)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app config))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme config")

(define tmp (mktempdir '(prefix . "aufnahme-config")))
(define (tmp-file name content)
  (let ((path (string-append tmp "/" name)))
    (call-with-output-file path (lambda (p) (write-string content p)))
    path))

(test-group "read-config"
  (let
      ((cfg (read-config (tmp-file "a.scm"
                                   "(define port 8071)\n(define ttl (* 60 60))\n(define name \"x\")"))))
    (test-equal 8071 (config-ref cfg 'port 0))
    (test-equal 3600 (config-ref cfg 'ttl 0))
    (test-equal "x" (config-ref cfg 'name #f))
    (test-equal 'none (config-ref cfg 'missing 'none)))
  (test-error (read-config (tmp-file "b.scm" "(display 1)"))))

(test-group "the example config reads"
  (let ((cfg (read-config (string-append root "/config.example.scm"))))
    (test-equal 8073 (config-ref cfg 'http-port 0))
    (test-equal "" (config-ref cfg 'url-prefix #f))
    (test-equal "var" (config-ref cfg 'data-dir #f))))

(test-group "a (load ...) form reads another config (Debian layout)"
  (let* ((real (tmp-file "etc-config.scm"
                         "(define http-port 9000)\n(define data-dir \"/var/lib/aufnahme\")"))
         (stub (tmp-file "app-config.scm"
                         (string-append
                          ";; Tools under bin/ load this\n(load \"" real
                          "\")\n")))
         (cfg (read-config stub)))
    (test-equal 9000 (config-ref cfg 'http-port 0))
    (test-equal "/var/lib/aufnahme" (config-ref cfg 'data-dir #f))))

(test-group "create-config!"
  (let ((ex (tmp-file "example.scm"
                      "(define secret \"REPLACE_WITH_BASE64_SECRET\")"))
        (target (string-append tmp "/config.scm")))
    (create-config! ex target)
    (let ((s (config-ref (read-config target) 'secret #f)))
      (test-assert "secret replaced"
                   (not (string=? s "REPLACE_WITH_BASE64_SECRET")))
      (test-equal 44 (string-length s)))
    (test-error "no overwrite" (create-config! ex target))
    (delete-file target)
    (delete-file ex)))

(test-group "load-config creates config.scm on the first run"
  (let ((app (string-append tmp "/app")))
    (make-directory app)
    (call-with-output-file (string-append app "/config.example.scm")
      (lambda (p) (write-string "(define http-port 1234)" p)))
    (test-equal 1234 (config-ref (load-config app '()) 'http-port 0))
    (test-assert (file-exists? (string-append app "/config.scm")))
    (delete-file (string-append app "/config.scm"))
    (delete-file (string-append app "/config.example.scm"))
    (delete-directory app)))

(test-group "config-secret"
  (test-equal "abc" (config-secret '((s . "abc")) 's))
  (test-equal '("new" "old") (config-secret '((s . ("new" "old"))) 's))
  (test-error (config-secret '() 's))
  (test-error (config-secret '((s . "REPLACE_WITH_BASE64_SECRET")) 's))
  (test-error (config-secret '((s . ("new" "REPLACE_WITH_BASE64_SECRET")))
                             's)))

(test-group "resolve-path"
  (test-equal "/etc/x" (resolve-path "/app" "/etc/x"))
  (test-equal "/app/data/x" (resolve-path "/app" "data/x")))

(delete-file (string-append tmp "/a.scm"))
(delete-file (string-append tmp "/b.scm"))
(delete-directory tmp)

(test-end "aufnahme config")
