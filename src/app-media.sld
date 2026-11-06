;; (app media) — ffprobe/ffmpeg work for recordings, run by one background
;; worker thread.
;;
;; After a take is recorded, process-take! remuxes each raw track
;; (screen.part, cam.part: MediaRecorder output without an index) into a
;; seekable file (-c copy, lossless), reads its length and size with
;; ffprobe, makes a poster frame, and computes the webcam offset from the
;; start times the browser reported.
;;
;; export-project! renders the project's takes with (app timeline) into
;; export.mp4, reporting progress (0..1) as ffmpeg runs.
;; Every ffmpeg run has a timeout; commands are argument lists, never
;; shell strings.

(define-library (app media)
  (import (scheme base)
          (scheme file)
          (srfi 18)
          (scm fs)
          (scm io)
          (scm json simple)
          (scm log)
          (scm string)
          (scm system)
          (app projects)
          (app store)
          (app timeline))
  (export enqueue-job!
          export-project!
          ffmpeg-program
          ffprobe-program
          jobs-idle?
          make-jobs
          probe-media
          process-take!
          project-segments)
  (begin

    (define ffmpeg-program (make-parameter "ffmpeg"))
    (define ffprobe-program (make-parameter "ffprobe"))

    (define (tail s n)
      (if (> (string-length s) n)
          (substring s (- (string-length s) n) (string-length s))
          s))

    (define (num x)
      (cond ((number? x) x)
            ((string? x) (or (string->number x) 0))
            (else 0)))

    ;; Runs ffmpeg with args; calls (tick) about twice a second while it
    ;; runs. Raises with the end of ffmpeg's output on failure or timeout.
    (define (run-ffmpeg args timeout-seconds tick)
      (let* ((log (mktemp '(prefix . "aufnahme-ffmpeg")))
             (h (start-program (cons (ffmpeg-program) args)
                               `((log-file ,log))))
             (deadline (+ (now-seconds) timeout-seconds)))
        (let loop ()
          (let ((code (process-wait h 500)))
            (cond (code (let
                            ((out (if (file-exists? log)
                                      (read-file-string log)
                                      "")))
                          (when (file-exists? log) (delete-file log))
                          (unless (= code 0)
                            (error (string-append "ffmpeg failed: "
                                                  (tail out 600))))))
                  ((> (now-seconds) deadline)
                   (process-kill h #t)
                   (process-wait h 5000)
                   (when (file-exists? log) (delete-file log))
                   (error "ffmpeg timed out" timeout-seconds))
                  (else (tick) (loop)))))))

    (define (probe-media path)
      "Syntax: (probe-media path)
Library: (app media)
Description: An alist (duration width height has-video has-audio) read
  with ffprobe. Raises if ffprobe fails."
      (let
          ((r (run-program/capture (list (ffprobe-program)
                                         "-v"
                                         "error"
                                         "-of"
                                         "json"
                                         "-show_format"
                                         "-show_streams"
                                         path))))
        (unless (and r (= (car r) 0))
          (error (string-append "ffprobe failed: "
                                (if r
                                    (tail (caddr r) 300)
                                    "not installed?"))))
        (let* ((j (json-parse (cadr r)))
               (streams (vector->list (json-ref j "streams" #())))
               (of-type (lambda (t)
                          (let loop ((ss streams))
                            (cond ((null? ss) #f)
                                  ((equal? (json-ref (car ss) "codec_type" "")
                                           t)
                                   (car ss))
                                  (else (loop (cdr ss)))))))
               (v (of-type "video"))
               (a (of-type "audio")))
          `((duration .
                      ,(num (json-ref (json-ref j "format" '())
                                      "duration"
                                      0)))
            (width . ,(if v (num (json-ref v "width" 0)) 0))
            (height . ,(if v (num (json-ref v "height" 0)) 0))
            (has-video . ,(and v #t))
            (has-audio . ,(and a #t))))))

    ;; --- takes ----------------------------------------------------------------

    (define (remux! data-dir pid tid track mime)
      ;; screen.part -> screen.webm (or .mp4): same streams, with an index
      ;; and a duration. Returns the new file name.
      (let* ((ext (track-extension mime))
             (name (string-append (symbol->string track) "." ext))
             (src (take-file data-dir pid tid
                             (string-append (symbol->string track) ".part")))
             (tmp (take-file data-dir pid tid
                             (string-append (symbol->string track)
                                            ".tmp."
                                            ext)))
             (dest (take-file data-dir pid tid name)))
        (guard (e (#t (when (file-exists? tmp) (delete-file tmp)) (raise e)))
          (run-ffmpeg (append (list "-nostdin" "-y" "-v" "error" "-fflags"
                                    "+genpts" "-i" src "-map" "0" "-c" "copy")
                              (if (string=? ext "mp4")
                                  (list "-movflags" "+faststart")
                                  '())
                              (list tmp))
                      (+ 120 (quotient (file-size src) 5000000))
                      (lambda () #f))
          (unless (move-file tmp dest) (error "cannot write" dest)))
        name))

    (define (process-take! data-dir pid tid)
      "Syntax: (process-take! data-dir pid tid)
Library: (app media)
Description: Remuxes and probes the tracks of a finished take, makes its
  poster frame and records the result (status ready or failed)."
      (guard (e (#t
                 (let
                     ((msg (if (error-object? e)
                               (error-object-message e)
                               "processing failed")))
                   (log-warn "media" (string-append pid "/" tid ": " msg))
                   (take-update! data-dir pid tid
                                 (lambda (t)
                                   (alist-set (alist-set t 'status 'failed)
                                              'error
                                              msg))))))
        (let* ((t (take-ref data-dir pid tid))
               (screen (alist-ref t 'screen))
               (cam (alist-ref t 'cam))
               (cam? (and cam (> (alist-ref cam 'bytes 0) 0)))
               (sname (remux! data-dir pid tid
                              'screen
                              (alist-ref screen 'mime)))
               (sinfo (probe-media (take-file data-dir pid tid sname)))
               (cname (and cam?
                           (guard (e (#t #f))
                             (remux! data-dir pid tid
                                     'cam
                                     (alist-ref cam 'mime)))))
               (cinfo (and cname
                           (guard (e (#t #f))
                             (probe-media
                              (take-file data-dir pid tid cname))))))
          (unless (alist-ref sinfo 'has-video)
            (error "no video in the screen recording"))
          (let ((poster (take-file data-dir pid tid "thumb.jpg")))
            (guard (e (#t (log-warn "media" "no poster frame")))
              (run-ffmpeg (list "-nostdin" "-y" "-v" "error" "-ss"
                                (number->string
                                 (min 1 (/ (alist-ref sinfo 'duration 0) 2.)))
                                "-i"
                                (take-file data-dir pid tid sname)
                                "-frames:v"
                                "1"
                                "-vf"
                                "scale='min(480,iw)':-2"
                                "-q:v"
                                "4"
                                poster)
                          60
                          (lambda () #f))))
          (take-update! data-dir pid tid
                        (lambda (t)
                          (let* ((t (alist-set
                                     t
                                     'screen
                                     (alist-set (alist-ref t 'screen)
                                                'file
                                                sname)))
                                 (t (alist-set
                                     t
                                     'cam
                                     (and
                                      (alist-ref t 'cam)
                                      (alist-set (alist-ref t 'cam)
                                                 'file
                                                 (and cinfo cname)))))
                                 (t (alist-set t
                                               'duration
                                               (alist-ref sinfo 'duration)))
                                 (t (alist-set t
                                               'width
                                               (alist-ref sinfo 'width)))
                                 (t (alist-set t
                                               'height
                                               (alist-ref sinfo 'height)))
                                 (t (alist-set t
                                               'has-audio
                                               (alist-ref sinfo 'has-audio)))
                                 (t (alist-set
                                     t
                                     'cam-offset
                                     (let
                                         ((s (alist-ref screen 'start))
                                          (c
                                           (and cam (alist-ref cam 'start))))
                                       (if (and (number? s) (number? c))
                                           (/ (- c s) 1000.)
                                           0))))
                                 (t (alist-set t 'status 'ready)))
                            (alist-set t 'error #f))))
          ;; The raw parts are no longer needed: the remuxed files hold the
          ;; same streams.
          (for-each (lambda (n)
                      (let ((p (take-file data-dir pid tid n)))
                        (when (file-exists? p) (delete-file p))))
                    (append (list "screen.part")
                            (if cname (list "cam.part") '())))
          (log-info "media" (string-append pid "/" tid ": ready")))))

    ;; --- export ------------------------------------------------------------------

    (define (project-segments data-dir p)
      "Syntax: (project-segments data-dir project)
Library: (app media)
Description: One whole-take segment (see (app timeline)) per ready take
  of project, in order."
      (let ((pid (alist-ref p 'id)))
        (let loop ((ts (project-takes data-dir p)))
          (cond ((null? ts) '())
                ((and (eq? (alist-ref (car ts) 'status) 'ready)
                      (> (alist-ref (car ts) 'duration 0) 0))
                 (let* ((t (car ts))
                        (tid (alist-ref t 'id))
                        (cam (alist-ref t 'cam))
                        (cfile (and cam (alist-ref cam 'file))))
                   (cons `((take . ,tid)
                           (screen .
                                   ,(take-file
                                     data-dir
                                     pid
                                     tid
                                     (alist-ref (alist-ref t 'screen) 'file)))
                           (cam .
                                ,(and cfile
                                      (take-file data-dir pid tid cfile)))
                           (cam-offset . ,(alist-ref t 'cam-offset 0))
                           (has-audio . ,(alist-ref t 'has-audio #f))
                           (width . ,(alist-ref t 'width 0))
                           (height . ,(alist-ref t 'height 0))
                           (start . 0)
                           (end . ,(alist-ref t 'duration 0)))
                         (loop (cdr ts)))))
                (else (loop (cdr ts)))))))

    (define (set-export! data-dir pid . kvs)
      (project-update! data-dir pid
                       (lambda (p)
                         (alist-set p
                                    'export
                                    (let loop
                                        ((e (alist-ref p 'export '()))
                                         (kvs kvs))
                                      (if
                                       (null? kvs)
                                       e
                                       (loop
                                        (alist-set e (car kvs) (cadr kvs))
                                        (cddr kvs))))))))

    ;; The last out_time_us=... value in ffmpeg's -progress file, in seconds.
    (define (progress-seconds path)
      (if (not (file-exists? path))
          0
          (let loop ((ls (string-split-lines (read-file-string path)))
                     (last 0))
            (cond ((null? ls) last)
                  ((and (> (string-length (car ls)) 12)
                        (string=? (substring (car ls) 0 12) "out_time_us="))
                   (loop (cdr ls)
                         (/ (num (substring (car ls)
                                            12
                                            (string-length (car ls))))
                            1000000.)))
                  (else (loop (cdr ls) last))))))

    (define (export-project! data-dir pid options)
      "Syntax: (export-project! data-dir pid options)
Library: (app media)
Description: Renders project pid into export.mp4 (see export-arguments
  for options), updating the project's export status and progress."
      (let* ((p (project-ref data-dir pid))
             (segs (if p (project-segments data-dir p) '()))
             (out (project-file data-dir pid "export.mp4"))
             (tmp (project-file data-dir pid "export.tmp.mp4"))
             (progress (project-file data-dir pid "export.progress"))
             (total (segments-length segs)))
        (guard (e (#t
                   (let
                       ((msg (if (error-object? e)
                                 (error-object-message e)
                                 "export failed")))
                     (log-warn "media" (string-append pid ": export: " msg))
                     (for-each (lambda (f)
                                 (when (file-exists? f) (delete-file f)))
                               (list tmp progress))
                     (set-export! data-dir pid 'status 'failed 'error msg))))
          (when (null? segs)
            (error "Keine fertigen Aufnahmen zum Exportieren."))
          (set-export! data-dir pid 'status 'running 'progress 0 'error #f)
          (let ((last-written 0))
            (run-ffmpeg (export-arguments segs
                                          (alist-ref p 'webcam '())
                                          options
                                          tmp
                                          progress)
                        (+ 600 (* 6 (exact (ceiling total))))
                        (lambda ()
                          ;; Write the progress at most every 2 s.
                          (when (>= (- (now-seconds) last-written) 2)
                            (set! last-written (now-seconds))
                            (set-export!
                             data-dir
                             pid
                             'progress
                             (min 0.99
                                  (/ (progress-seconds progress)
                                     (max total 0.1))))))))
          (unless (move-file tmp out) (error "cannot write export"))
          (when (file-exists? progress) (delete-file progress))
          (set-export! data-dir pid
                       'status
                       'ready
                       'progress
                       1
                       'finished
                       (now-seconds)
                       'duration
                       total
                       'size
                       (file-size out))
          (log-info "media" (string-append pid ": exported")))))

    ;; --- the job queue -------------------------------------------------------------

    (define-record-type jobs
      (%make-jobs mutex cv queue busy)
      jobs?
      (mutex jobs-mutex)
      (cv jobs-cv)
      (queue jobs-queue set-jobs-queue!)
      (busy jobs-busy set-jobs-busy!))

    (define (make-jobs)
      "Syntax: (make-jobs)
Library: (app media)
Description: Starts the worker thread that runs queued jobs one by one."
      (let ((q (%make-jobs (make-mutex) (make-condition-variable) '() #f)))
        (thread-start! (make-thread (lambda ()
                                      (let loop ()
                                        (mutex-lock! (jobs-mutex q))
                                        (let wait ()
                                          (when (null? (jobs-queue q))
                                            (mutex-unlock! (jobs-mutex q)
                                                           (jobs-cv q))
                                            (mutex-lock! (jobs-mutex q))
                                            (wait)))
                                        (let ((job (car (jobs-queue q))))
                                          (set-jobs-queue!
                                           q
                                           (cdr (jobs-queue q)))
                                          (set-jobs-busy! q #t)
                                          (mutex-unlock! (jobs-mutex q))
                                          (guard
                                              (e
                                               (#t
                                                (log-error
                                                 "media"
                                                 (string-append "job failed: "
                                                                (car job)))))
                                            ((cdr job)))
                                          (set-jobs-busy! q #f))
                                        (loop)))
                                    "aufnahme-jobs"))
        q))

    (define (enqueue-job! q key thunk)
      "Syntax: (enqueue-job! jobs key thunk)
Library: (app media)
Description: Queues thunk under key (a string); a job with the same key
  that is still waiting is not queued twice. With jobs #f, runs thunk
  right away (tests, CLI)."
      (if (not q)
          (thunk)
          (begin
            (mutex-lock! (jobs-mutex q))
            (unless (assoc key (jobs-queue q))
              (set-jobs-queue! q
                               (append (jobs-queue q)
                                       (list (cons key thunk)))))
            (condition-variable-signal! (jobs-cv q))
            (mutex-unlock! (jobs-mutex q)))))

    (define (jobs-idle? q)
      "Syntax: (jobs-idle? jobs)
Library: (app media)
Description: True when nothing is queued or running."
      (or (not q) (and (not (jobs-busy q)) (null? (jobs-queue q)))))))
