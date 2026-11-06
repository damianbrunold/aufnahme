;; (app store) — small helpers for the file-based data folder.
;;
;; Every data file is one Scheme datum (an alist or a list), written with
;; write and read back with read. Writes go to a temp file in the same
;; directory, then are renamed over the target, so a reader (or a backup)
;; never sees a half-written file. A process-wide mutex serializes the
;; app's own writes; separate processes (the CLI) take an OS file lock on
;; data-dir/.lock via with-data-lock.

(define-library (app store)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme read)
          (scheme time)
          (scheme write)
          (srfi 18)
          (scm crypto)
          (scm fs))
  (export alist-ref
          alist-set
          ensure-dir
          new-id
          now-seconds
          read-datum
          valid-id?
          with-data-lock
          write-datum!)
  (begin

    (define (alist-ref al key . default)
      "Syntax: (alist-ref alist key [default])
Library: (app store)
Description: The value under symbol key in alist, or default (#f)."
      (let ((p (assq key al)))
        (cond (p (cdr p))
              ((pair? default) (car default))
              (else #f))))

    (define (alist-set al key value)
      "Syntax: (alist-set alist key value)
Library: (app store)
Description: A copy of alist with key bound to value (replaced in place,
  or appended)."
      (let loop ((xs al))
        (cond ((null? xs) (list (cons key value)))
              ((eq? (caar xs) key) (cons (cons key value) (cdr xs)))
              (else (cons (car xs) (loop (cdr xs)))))))

    (define (now-seconds)
      "Syntax: (now-seconds)
Library: (app store)
Description: The current time as whole seconds since the epoch."
      (exact (floor (current-second))))

    (define (ensure-dir path)
      "Syntax: (ensure-dir path)
Library: (app store)
Description: Creates directory path and its parents if missing."
      (unless (directory-exists? path)
        (let ((parent (directory-name path)))
          (when
              (and parent
                   (not (string=? parent path))
                   (> (string-length parent) 0))
            (ensure-dir parent)))
        (unless (directory-exists? path) (make-directory path))))

    (define (read-datum path default)
      "Syntax: (read-datum path default)
Library: (app store)
Description: The datum stored in path, or default when the file is
  missing or empty."
      (if (file-exists? path)
          (let ((d (call-with-input-file path read)))
            (if (eof-object? d) default d))
          default))

    (define write-mutex (make-mutex))

    (define (write-datum! path datum)
      "Syntax: (write-datum! path datum)
Library: (app store)
Description: Atomically replaces path with datum: writes path.tmp in the
  same directory and renames it over path."
      (let ((tmp (string-append path ".tmp")))
        (ensure-dir (directory-name path))
        (dynamic-wind
          (lambda () (mutex-lock! write-mutex))
          (lambda ()
            (call-with-output-file tmp
              (lambda (p)
                (write datum p)
                (newline p)))
            (unless (move-file tmp path)
              (error "write-datum!: cannot rename" tmp path)))
          (lambda () (mutex-unlock! write-mutex)))))

    (define (with-data-lock data-dir thunk)
      "Syntax: (with-data-lock data-dir thunk)
Library: (app store)
Description: Calls thunk while holding the OS lock on data-dir/.lock,
  waiting (up to 30 s) while another process holds it."
      (ensure-dir data-dir)
      (let ((path (string-append data-dir "/.lock")))
        (let loop ((tries 0))
          (let ((h (file-lock path)))
            (cond (h (dynamic-wind
                       (lambda () #f)
                       thunk
                       (lambda () (file-unlock h))))
                  ((< tries 300) (thread-sleep! 0.1) (loop (+ tries 1)))
                  (else (error "data dir is locked" path)))))))

    ;; 8 random bytes -> 11 URL-safe characters (base64url, no padding).
    (define (new-id)
      "Syntax: (new-id)
Library: (app store)
Description: A fresh random id of 11 URL-safe characters."
      (let ((s (base64-encode (random-bytes 8))))
        (list->string (let loop ((cs (string->list s)))
                        (cond ((null? cs) '())
                              ((char=? (car cs) #\=) '())
                              ((char=? (car cs) #\+)
                               (cons #\- (loop (cdr cs))))
                              ((char=? (car cs) #\/)
                               (cons #\_ (loop (cdr cs))))
                              (else (cons (car cs) (loop (cdr cs)))))))))

    (define (valid-id? s)
      "Syntax: (valid-id? s)
Library: (app store)
Description: True if s looks like an id from new-id (so it is safe to use
  as a directory name)."
      (and (string? s)
           (<= 4 (string-length s) 32)
           (let loop ((i 0))
             (or (= i (string-length s))
                 (let ((c (string-ref s i)))
                   (and (or (and (char<=? #\a c) (char<=? c #\z))
                            (and (char<=? #\A c) (char<=? c #\Z))
                            (char-numeric? c)
                            (memv c '(#\- #\_)))
                        (loop (+ i 1))))))))))
