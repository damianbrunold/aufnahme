;; Tests for projects/takes storage and the export command builder. No
;; server, no ffmpeg.
;; Run: scm tests/test_pure_projects.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm fs)
        (scm module)
        (scm string)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app projects)
        (app store)
        (app timeline))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme projects")

(define tmp (mktempdir '(prefix . "aufnahme-projects")))

(define (has? s part) (and (string-contains-from s part 0) #t))
(define (file-bytes path)
  (let* ((port (open-binary-input-file path))
         (bv (read-bytevector 1000000 port)))
    (close-port port)
    (if (eof-object? bv) (bytevector) bv)))

(test-group "projects"
  (let* ((p (project-create! tmp "uli" "Erstes"))
         (pid (alist-ref p 'id)))
    (test-equal "Erstes" (alist-ref (project-ref tmp pid) 'title))
    (test-equal "uli" (alist-ref p 'owner))
    (test-equal '() (alist-ref p 'takes))
    (test-equal 'none (alist-ref (alist-ref p 'export) 'status))
    (test-equal 1 (length (project-list tmp)))
    (project-update! tmp pid (lambda (p) (alist-set p 'title "Neu")))
    (test-equal "Neu" (alist-ref (project-ref tmp pid) 'title))
    (test-equal #f (project-ref tmp "../../etc"))
    (test-equal #f (project-update! tmp "nosuchproject" (lambda (p) p)))
    (project-delete! tmp pid)
    (test-equal #f (project-ref tmp pid))))

(test-group "takes and chunks"
  (let* ((p (project-create! tmp "uli" "Chunks"))
         (pid (alist-ref p 'id))
         (t (take-create! tmp pid "video/webm;codecs=vp9,opus"
                          "video/webm;codecs=vp9"))
         (tid (alist-ref t 'id))
         (t2 (take-create! tmp pid "video/mp4" #f)))
    (test-equal (list tid (alist-ref t2 'id))
                (alist-ref (project-ref tmp pid) 'takes))
    (test-equal 'recording (alist-ref t 'status))
    (test-equal #f (alist-ref t2 'cam))
    (test-equal 1 (take-append! tmp pid tid 'screen 0 (bytevector 1 2 3)))
    (test-equal "a repeated chunk is ignored" 1
                (take-append! tmp pid tid 'screen 0 (bytevector 1 2 3)))
    (test-equal '(conflict . 1)
                (take-append! tmp pid tid 'screen 5 (bytevector 9)))
    (test-equal 2 (take-append! tmp pid tid 'screen 1 (bytevector 4 5)))
    (test-equal (bytevector 1 2 3 4 5)
                (file-bytes (take-file tmp pid tid "screen.part")))
    (test-equal 1 (take-append! tmp pid tid 'cam 0 (bytevector 7)))
    (test-equal "no webcam track" #f
                (take-append! tmp pid
                              (alist-ref t2 'id)
                              'cam
                              0
                              (bytevector 7)))
    (let ((tr (alist-ref (take-ref tmp pid tid) 'screen)))
      (test-equal 2 (alist-ref tr 'chunks))
      (test-equal 5 (alist-ref tr 'bytes)))
    (take-update! tmp pid tid (lambda (t) (alist-set t 'status 'processing)))
    (test-equal "no chunks once finished" #f
                (take-append! tmp pid tid 'screen 2 (bytevector 6)))
    (test-equal 2 (length (project-takes tmp (project-ref tmp pid))))
    (take-delete! tmp pid (alist-ref t2 'id))
    (test-equal (list tid) (alist-ref (project-ref tmp pid) 'takes))
    (test-assert (not (file-exists? (take-dir tmp pid (alist-ref t2 'id)))))
    (test-equal "mp4" (track-extension "video/mp4;codecs=avc1"))
    (test-equal "webm" (track-extension "video/webm;codecs=vp8,opus"))))

(define (seg . kvs)
  (append kvs
          '((screen . "/s.webm")
            (cam . #f)
            (cam-offset . 0)
            (has-audio . #t)
            (start . 0)
            (end . 10)
            (width . 1920)
            (height . 1080))))

(define (joined args)
  (let loop ((as args) (acc ""))
    (if (null? as) acc (loop (cdr as) (string-append acc " " (car as))))))

(test-group "timeline"
  (test-equal 10 (segment-length (seg)))
  (test-equal 13.5
              (segments-length (list (seg) (seg '(start . 1) '(end . 4.5)))))
  (test-equal '(1920 . 1080) (output-size (list (seg)) 1920))
  (test-equal '(1920 . 1200)
              (output-size (list (seg '(width . 2560) '(height . 1600)))
                           1920))
  (test-equal '(1280 . 720)
              (output-size (list (seg '(width . 1279) '(height . 719))) 1920))
  (let* ((args (export-arguments (list (seg '(cam . "/c.webm")
                                            '(cam-offset . 0.25)
                                            '(start . 2)
                                            '(end . 8))
                                       (seg '(screen . "/t.webm")
                                            '(has-audio . #f)))
                                 '((on . #t)
                                   (corner . top-left)
                                   (size . 0.25)
                                   (margin . 10))
                                 '((preset . "medium") (crf . 20))
                                 "/out.mp4"
                                 "/prog"))
         (s (joined args)))
    (test-assert "seeked screen input"
                 (has? s "-ss 2.000 -to 8.000 -i /s.webm"))
    (test-assert "webcam input" (has? s "-i /c.webm"))
    (test-assert "second take" (has? s "-ss 0.000 -to 10.000 -i /t.webm"))
    (test-assert "webcam trimmed by the offset"
                 (has? s "trim=start=1.750:end=7.750"))
    (test-assert "webcam width" (has? s "scale=480:-2[c0]"))
    (test-assert "corner" (has? s "overlay=10:10:eof_action=pass"))
    (test-assert "exact segment length" (has? s "trim=duration=6.000"))
    (test-assert "silence for a take without audio"
                 (has? s "anullsrc=r=48000:cl=stereo,atrim=duration=10.000"))
    (test-assert "inputs: screen 0, cam 1, screen 2" (has? s "[2:v]setpts"))
    (test-assert (has? s "[v0][a0][v1][a1]concat=n=2:v=1:a=1[v][a0]"))
    (test-assert (has? s "-preset medium -crf 20"))
    (test-assert (has? s "-movflags +faststart"))
    (test-assert (has? s "-progress /prog"))
    (test-equal "/out.mp4" (list-ref args (- (length args) 1))))
  (let
      ((s (joined (export-arguments (list
                                     (seg '(cam . "/c.webm")
                                          '(cam-offset . -0.5)))
                                    '((on . #t))
                                    '((loudnorm . #t))
                                    "/o.mp4"
                                    "/p"))))
    (test-assert "webcam started earlier: skip its first half second"
                 (has? s "trim=start=0.500:end=10.500"))
    (test-assert "default corner" (has? s "overlay=W-w-24:H-h-24"))
    (test-assert (has? s "loudnorm")))
  (let
      ((s (joined (export-arguments (list
                                     (seg '(cam . "/c.webm")
                                          '(cam-offset . 2)
                                          '(start . 1)))
                                    '((on . #t))
                                    '()
                                    "/o.mp4"
                                    "/p"))))
    (test-assert "webcam started later: delay it"
                 (has? s
                       "trim=start=0.000:end=8.000,setpts=PTS-STARTPTS+1.000/TB")))
  (test-assert "webcam off: not used"
               (not (has? (joined (export-arguments
                                   (list (seg '(cam . "/c.webm")))
                                   '((on . #f))
                                   '()
                                   "/o.mp4"
                                   "/p"))
                          "/c.webm"))))

(test-group "cut list"
  (define takes '(("a" . 10.0) ("b" . 4.5)))
  (test-equal '(((take . "a") (start . 0) (end . 10.0))
                ((take . "b") (start . 0) (end . 4.5)))
              (default-segments takes))
  (test-equal "no cut list: every take whole"
              (default-segments takes)
              (clean-segments #f takes))
  (test-equal "kept as given"
              '(((take . "b") (start . 1.0) (end . 2.0))
                ((take . "a") (start . 3.0) (end . 4.0)))
              (clean-segments '(((take . "b") (start . 1) (end . 2))
                                ((take . "a") (start . 3) (end . 4)))
                              takes))
  (test-equal "clamped to the take"
              '(((take . "b") (start . 0.0) (end . 4.5)))
              (clean-segments '(((take . "b") (start . -3) (end . 99)))
                              takes))
  (test-equal "unknown takes, junk and tiny pieces are dropped"
              '(((take . "a") (start . 1.0) (end . 1.5)))
              (clean-segments '(((take . "zz") (start . 0) (end . 1))
                                ((take . "a") (start . 2) (end . 2.01))
                                ((take . "a") (start . "x") (end . 3))
                                "garbage"
                                ((take . "a") (start . 1) (end . 1.5)))
                              takes))
  (test-equal "rounded to milliseconds"
              '(((take . "a") (start . 1.235) (end . 2.0)))
              (clean-segments '(((take . "a") (start . 1.23456) (end . 2)))
                              takes))
  (test-equal '() (clean-segments '() takes))
  (test-equal 4 (strip-count 3))
  (test-equal 20 (strip-count 100))
  (test-equal 120 (strip-count 3600)))

(rm tmp 'recursive 'force)
(test-end "aufnahme projects")
