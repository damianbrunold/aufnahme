;; Usage: scm bin/maintenance.scm [--config path]
;;
;; Nightly housekeeping (aufnahme-maintenance.timer):
;;   - removes temporary files older than a day that an interrupted remux,
;;     preview or export left behind (*.tmp.*, export.progress);
;;   - if export-max-age-days > 0: deletes exports older than that (they
;;     can be rendered again from the recordings at any time);
;;   - if project-max-age-days > 0: deletes whole projects (recordings
;;     included) not changed for that long.
;; Both age settings default to 0: keep everything. Safe to run while the
;; server runs.

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm fs)
        (scm log)
        (scm module)
        (scm string))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app cli)
        (app config)
        (app projects)
        (app store))

(define parsed (cli-config root (cdr (command-line))))
(define cfg (car parsed))
(define data-dir (cli-data-dir root cfg))
(define day (* 24 3600))
(define now (now-seconds))

(define (age-days path)
  (/ (- now (quotient (file-modification-timestamp path) 1000)) day))

(define (temp-file? name)
  (or (string-contains-from name ".tmp." 0)
      (string=? name "export.progress")))

(define removed-temp 0)
(define removed-exports 0)
(define removed-projects 0)

(define (sweep-temp dir)
  (when (directory-exists? dir)
    (for-each (lambda (f)
                (let ((p (string-append dir "/" f)))
                  (when (and (temp-file? f) (> (age-days p) 1))
                    (delete-file p)
                    (set! removed-temp (+ removed-temp 1)))))
              (directory-files dir))))

(define export-days (config-ref cfg 'export-max-age-days 0))
(define project-days (config-ref cfg 'project-max-age-days 0))

(for-each (lambda (p)
            (let ((pid (alist-ref p 'id)))
              (cond ((and (> project-days 0)
                          (> (/ (- now (alist-ref p 'updated now)) day)
                             project-days))
                     (project-delete! data-dir pid)
                     (set! removed-projects (+ removed-projects 1)))
                    (else (sweep-temp (project-dir data-dir pid))
                          (for-each (lambda (tid)
                                      (sweep-temp
                                       (take-dir data-dir pid tid)))
                                    (alist-ref p 'takes '()))
                          (let
                              ((export (project-file data-dir pid
                                                     "export.mp4")))
                            (when
                                (and (> export-days 0)
                                     (file-exists? export)
                                     (> (age-days export) export-days))
                              (delete-file export)
                              (project-update!
                               data-dir
                               pid
                               (lambda (p)
                                 (alist-set
                                  p
                                  'export
                                  '((status . none)
                                    (progress . 0)
                                    (error . #f)
                                    (finished . #f)))))
                              (set! removed-exports
                                    (+ removed-exports 1))))))))
          (project-list data-dir))

(log-info "maintenance"
          (string-append "removed "
                         (number->string removed-temp)
                         " temporary file(s), "
                         (number->string removed-exports)
                         " export(s), "
                         (number->string removed-projects)
                         " project(s)"))
