;; Tests for bin/maintenance.scm, run as a separate process on a temporary
;; data folder. No ffmpeg needed.
;; Run: scm tests/test_maintenance.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (scm fs)
        (scm module)
        (scm system)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app projects)
        (app store))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme maintenance")

(define tmp (mktempdir '(prefix . "aufnahme-maint")))
(define data (string-append tmp "/data"))

(define (config export-days project-days)
  (let ((path (string-append tmp "/config.scm")))
    (call-with-output-file path
      (lambda (p)
        (write `(define data-dir ,data) p)
        (write `(define export-max-age-days ,export-days) p)
        (write `(define project-max-age-days ,project-days) p)))
    path))

(define (maintain export-days project-days)
  (let
      ((r (run-program/capture (list "scm"
                                     (string-append root
                                                    "/bin/maintenance.scm")
                                     "--config"
                                     (config export-days project-days)))))
    (and r (= 0 (car r)))))

(define (touch path . days-old)
  (call-with-output-file path (lambda (p) (write-string "x" p)))
  (when (pair? days-old)
    (run-program (list "touch" "-d"
                       (string-append (number->string (car days-old))
                                      " days ago")
                       path))))

(define old (project-create! data "uli" "Alt"))
(define new (project-create! data "uli" "Neu"))
(define old-id (alist-ref old 'id))
(define new-id (alist-ref new 'id))
(define t (take-create! data new-id "video/webm" #f))
(define tid (alist-ref t 'id))

;; Leftovers: an old temp file, a fresh one, an old export.
(touch (take-file data new-id tid "screen.tmp.webm") 3)
(touch (take-file data new-id tid "strip.tmp.jpg"))
(touch (project-file data new-id "export.progress") 2)
(touch (project-file data new-id "export.mp4") 10)
(project-update! data new-id
                 (lambda (p)
                   (alist-set p
                              'export
                              '((status . ready)
                                (progress . 1)
                                (finished . 1)))))
;; The old project was last changed 100 days ago.
(project-update! data old-id (lambda (p) p))
(write-datum! (project-file data old-id "project.scm")
              (alist-set (project-ref data old-id)
                         'updated
                         (- (now-seconds) (* 100 86400))))

(test-group "defaults keep everything but old temporary files"
  (test-assert (maintain 0 0))
  (test-assert "old temp file removed"
               (not (file-exists? (take-file data new-id tid
                                             "screen.tmp.webm"))))
  (test-assert "old progress file removed"
               (not (file-exists? (project-file data new-id
                                                "export.progress"))))
  (test-assert "fresh temp file kept"
               (file-exists? (take-file data new-id tid "strip.tmp.jpg")))
  (test-assert "export kept"
               (file-exists? (project-file data new-id "export.mp4")))
  (test-assert "old project kept" (project-ref data old-id))
  (test-assert "the take itself is untouched" (take-ref data new-id tid)))

(test-group "old exports"
  (test-assert (maintain 30 0))
  (test-assert "10 days is not old enough"
               (file-exists? (project-file data new-id "export.mp4")))
  (test-assert (maintain 7 0))
  (test-assert (not (file-exists? (project-file data new-id "export.mp4"))))
  (test-equal 'none
              (alist-ref (alist-ref (project-ref data new-id) 'export)
                         'status)))

(test-group "old projects"
  (test-assert (maintain 0 365))
  (test-assert (project-ref data old-id))
  (test-assert (maintain 0 90))
  (test-equal #f (project-ref data old-id))
  (test-assert "recent projects stay" (project-ref data new-id)))

(rm tmp 'recursive 'force)
(test-end "aufnahme maintenance")
