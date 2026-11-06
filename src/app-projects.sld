;; (app projects) — recording projects and their takes, stored as files.
;;
;;   data-dir/projects/<pid>/project.scm      the project (alist)
;;   data-dir/projects/<pid>/takes/<tid>/take.scm
;;   data-dir/projects/<pid>/takes/<tid>/screen.part, cam.part
;;                                            raw MediaRecorder output, as it
;;                                            arrives chunk by chunk
;;   .../takes/<tid>/screen.webm|mp4, cam.webm|mp4, thumb.jpg
;;                                            after processing (remuxed)
;;   data-dir/projects/<pid>/export.mp4       the last export
;;
;; Chunks carry a sequence number per track. A chunk is written at the
;; byte offset recorded in take.scm and only then counted, so a chunk sent
;; twice (a retry after a lost response) is ignored, and one written just
;; before a crash is simply written again at the same place.

(define-library (app projects)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 18)
          (srfi 132)
          (scm fs)
          (scm random access)
          (app store))
  (export project-create!
          project-delete!
          project-dir
          project-file
          project-list
          project-ref
          project-takes
          project-update!
          take-append!
          take-create!
          take-delete!
          take-dir
          take-file
          take-ref
          take-update!
          track-extension)
  (begin

    (define mutex (make-mutex))

    (define (locked thunk)
      (dynamic-wind
        (lambda () (mutex-lock! mutex))
        thunk
        (lambda () (mutex-unlock! mutex))))

    (define (projects-root data-dir) (string-append data-dir "/projects"))

    (define (project-dir data-dir pid)
      "Syntax: (project-dir data-dir pid)
Library: (app projects)
Description: The directory of project pid."
      (string-append (projects-root data-dir) "/" pid))

    (define (project-file data-dir pid name)
      "Syntax: (project-file data-dir pid name)
Library: (app projects)
Description: The path of file name in project pid (\"export.mp4\")."
      (string-append (project-dir data-dir pid) "/" name))

    (define (take-dir data-dir pid tid)
      "Syntax: (take-dir data-dir pid tid)
Library: (app projects)
Description: The directory of take tid of project pid."
      (string-append (project-dir data-dir pid) "/takes/" tid))

    (define (take-file data-dir pid tid name)
      "Syntax: (take-file data-dir pid tid name)
Library: (app projects)
Description: The path of file name (\"screen.part\") of a take."
      (string-append (take-dir data-dir pid tid) "/" name))

    (define (meta-path data-dir pid)
      (project-file data-dir pid "project.scm"))
    (define (take-meta-path data-dir pid tid)
      (take-file data-dir pid tid "take.scm"))

    ;; --- projects ----------------------------------------------------------

    (define (project-ref data-dir pid)
      "Syntax: (project-ref data-dir pid)
Library: (app projects)
Description: Project pid's alist, or #f."
      (and (valid-id? pid)
           (let
               ((p (guard (e (#t #f))
                     (read-datum (meta-path data-dir pid) #f))))
             (and (list? p) (equal? (alist-ref p 'id) pid) p))))

    (define (project-list data-dir)
      "Syntax: (project-list data-dir)
Library: (app projects)
Description: All projects, most recently changed first."
      (let ((root (projects-root data-dir)))
        (if (not (directory-exists? root))
            '()
            (list-sort (lambda (a b)
                         (> (alist-ref a 'updated 0)
                            (alist-ref b 'updated 0)))
                       (let loop ((ds (directory-directories root))
                                  (acc '()))
                         (if (null? ds)
                             acc
                             (let ((p (project-ref data-dir (car ds))))
                               (loop (cdr ds) (if p (cons p acc) acc)))))))))

    (define (project-create! data-dir owner title)
      "Syntax: (project-create! data-dir owner title)
Library: (app projects)
Description: Creates an empty project owned by owner (a username) and
  returns it."
      (locked (lambda ()
                (let loop ()
                  (let ((pid (new-id)))
                    (if (file-exists? (project-dir data-dir pid))
                        (loop)
                        (let* ((now (now-seconds))
                               (p `((version . 1)
                                    (id . ,pid)
                                    (title . ,title)
                                    (owner . ,owner)
                                    (created . ,now)
                                    (updated . ,now)
                                    (takes . ())
                                    (webcam (on . #t)
                                            (corner . bottom-right)
                                            (size . 0.22)
                                            (margin . 24))
                                    (export (status . none)
                                            (progress . 0)
                                            (error . #f)
                                            (finished . #f)))))
                          (ensure-dir (string-append
                                       (project-dir data-dir pid)
                                       "/takes"))
                          (write-datum! (meta-path data-dir pid) p)
                          p)))))))

    (define (project-update! data-dir pid proc)
      "Syntax: (project-update! data-dir pid proc)
Library: (app projects)
Description: Replaces project pid with (proc project), stamping updated.
  Returns the new project, or #f if there is none."
      (locked (lambda ()
                (let ((p (project-ref data-dir pid)))
                  (and p
                       (let ((n (alist-set (proc p) 'updated (now-seconds))))
                         (write-datum! (meta-path data-dir pid) n)
                         n))))))

    (define (project-delete! data-dir pid)
      "Syntax: (project-delete! data-dir pid)
Library: (app projects)
Description: Deletes project pid with all its recordings."
      (when
          (and (valid-id? pid) (directory-exists? (project-dir data-dir pid)))
        (locked (lambda ()
                  (rm (project-dir data-dir pid) 'recursive 'force)))))

    (define (project-takes data-dir p)
      "Syntax: (project-takes data-dir project)
Library: (app projects)
Description: The take alists of project, in order."
      (let loop ((ids (alist-ref p 'takes '())))
        (cond ((null? ids) '())
              ((take-ref data-dir (alist-ref p 'id) (car ids))
               =>
               (lambda (t) (cons t (loop (cdr ids)))))
              (else (loop (cdr ids))))))

    ;; --- takes ----------------------------------------------------------------

    (define (track-extension mime)
      "Syntax: (track-extension mime)
Library: (app projects)
Description: \"mp4\" for an MP4 recording, else \"webm\"."
      (if (and (string? mime)
               (>= (string-length mime) 9)
               (string=? (string-downcase (substring mime 0 9)) "video/mp4"))
          "mp4"
          "webm"))

    (define (take-ref data-dir pid tid)
      "Syntax: (take-ref data-dir pid tid)
Library: (app projects)
Description: Take tid's alist, or #f."
      (and (valid-id? pid)
           (valid-id? tid)
           (let
               ((t (guard (e (#t #f))
                     (read-datum (take-meta-path data-dir pid tid) #f))))
             (and (list? t) t))))

    (define (new-track mime start)
      `((mime . ,mime) (start . ,start) (chunks . 0) (bytes . 0) (file . #f)))

    (define (take-create! data-dir pid screen-mime cam-mime)
      "Syntax: (take-create! data-dir pid screen-mime cam-mime)
Library: (app projects)
Description: Adds a new take to project pid, recording the screen in
  screen-mime and the webcam in cam-mime (or #f: no webcam). Returns the
  take, or #f if there is no such project."
      (and (project-ref data-dir pid)
           (let* ((tid (let loop ()
                         (let ((t (new-id)))
                           (if (take-ref data-dir pid t) (loop) t))))
                  (t `((version . 1)
                       (id . ,tid)
                       (status . recording)
                       (error . #f)
                       (created . ,(now-seconds))
                       (screen . ,(new-track screen-mime #f))
                       (cam . ,(and cam-mime (new-track cam-mime #f)))
                       (duration . 0)
                       (width . 0)
                       (height . 0)
                       (has-audio . #f)
                       (cam-offset . 0))))
             (ensure-dir (take-dir data-dir pid tid))
             (locked (lambda ()
                       (write-datum! (take-meta-path data-dir pid tid) t)))
             (project-update! data-dir pid
                              (lambda (p)
                                (alist-set
                                 p
                                 'takes
                                 (append (alist-ref p 'takes '())
                                         (list tid)))))
             t)))

    (define (take-update! data-dir pid tid proc)
      "Syntax: (take-update! data-dir pid tid proc)
Library: (app projects)
Description: Replaces take tid with (proc take). Returns the new take, or
  #f if there is none."
      (locked (lambda ()
                (let ((t (take-ref data-dir pid tid)))
                  (and t
                       (let ((n (proc t)))
                         (write-datum! (take-meta-path data-dir pid tid) n)
                         n))))))

    (define (take-delete! data-dir pid tid)
      "Syntax: (take-delete! data-dir pid tid)
Library: (app projects)
Description: Removes take tid from project pid and deletes its files."
      (when (take-ref data-dir pid tid)
        (project-update! data-dir pid
                         (lambda (p)
                           (alist-set p
                                      'takes
                                      (let loop
                                          ((ids (alist-ref p 'takes '())))
                                        (cond
                                         ((null? ids) '())
                                         ((equal? (car ids) tid) (cdr ids))
                                         (else
                                          (cons (car ids)
                                                (loop (cdr ids)))))))))
        (locked (lambda ()
                  (rm (take-dir data-dir pid tid) 'recursive 'force)))))

    (define (take-append! data-dir pid tid track seq bytes)
      "Syntax: (take-append! data-dir pid tid track seq bytes)
Library: (app projects)
Description: Stores chunk number seq (0, 1, ...) of track ('screen or
  'cam). Returns the number of chunks stored; a repeated chunk is
  ignored. Returns (conflict . expected-seq) for a chunk out of order,
  #f if the take or track does not exist or is no longer recording."
      (locked (lambda ()
                (let* ((t (take-ref data-dir pid tid))
                       (tr (and t (alist-ref t track))))
                  (cond ((or (not tr)
                             (not (eq? (alist-ref t 'status) 'recording)))
                         #f)
                        ((< seq (alist-ref tr 'chunks))
                         (alist-ref tr 'chunks))
                        ((> seq (alist-ref tr 'chunks))
                         (cons 'conflict (alist-ref tr 'chunks)))
                        (else (let ((offset (alist-ref tr 'bytes))
                                    (path
                                     (take-file
                                      data-dir
                                      pid
                                      tid
                                      (string-append (symbol->string track)
                                                     ".part"))))
                                (call-with-random-access-file path
                                  'update
                                  (lambda (f)
                                    (random-access-file-write! f offset
                                                               bytes)))
                                (let
                                    ((tr
                                      (alist-set
                                       (alist-set tr 'chunks (+ seq 1))
                                       'bytes
                                       (+ offset (bytevector-length bytes)))))
                                  (write-datum!
                                   (take-meta-path data-dir pid tid)
                                   (alist-set t track tr))
                                  (+ seq 1)))))))))))
