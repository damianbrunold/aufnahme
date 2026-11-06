;; Tests that need ffmpeg/ffprobe on the PATH: a recording uploaded in
;; chunks through the API (like the browser does), processed, and exported
;; with the webcam overlay.
;; Run: scm tests/test_media.scm

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scm crypto)
        (scm crypto password)
        (scm fs)
        (scm log)
        (scm module)
        (scm net http request)
        (scm net http response)
        (scm random access)
        (scm string)
        (scm system)
        (scm test))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (app media)
        (app projects)
        (app store)
        (app users)
        (app web))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme media")

(define tmp (mktempdir '(prefix . "aufnahme-media")))

(define (ffmpeg . args)
  (run-program (append (list "ffmpeg" "-v" "error" "-y") args)))

;; What MediaRecorder would send: WebM with VP8 video (and Opus audio for
;; the screen). The webcam clip counts seconds, so the offset is visible.
(define screen (string-append tmp "/screen.webm"))
(define cam (string-append tmp "/cam.webm"))
(test-equal 0
            (ffmpeg "-f" "lavfi" "-i"
                    "testsrc2=duration=5:size=1280x720:rate=30" "-f" "lavfi"
                    "-i" "sine=frequency=440:duration=5" "-c:v" "libvpx"
                    "-b:v" "1M" "-c:a" "libopus" screen))
(test-equal 0
            (ffmpeg "-f" "lavfi" "-i"
                    "testsrc=duration=5:size=320x240:rate=30" "-c:v" "libvpx"
                    "-b:v" "300k" cam))

(define (file-bytes path)
  (call-with-random-access-file path
    'read
    (lambda (f) (random-access-file-read f 0 (file-size path)))))

;; Splits bv into n roughly equal chunks.
(define (chunks bv n)
  (let* ((len (bytevector-length bv))
         (size (quotient (+ len n -1) n)))
    (let loop ((i 0) (acc '()))
      (if (>= i len)
          (reverse acc)
          (loop (+ i size)
                (cons (bytevector-copy bv i (min len (+ i size))) acc))))))

(parameterize ((password-pbkdf2-iterations 1000))
  (save-user! (string-append tmp "/users.scm")
              "uli"
              (password-hash "uli password")
              'user))

(define app
  (make-app `((url-prefix . "")
              (static-dir . ,(string-append root "/static"))
              (data-dir . ,tmp)
              (session-secret . ,(base64-encode (random-bytes 32)))
              (session-max-age . 3600)
              (secure-cookies? . #f)
              (process? . #f)
              (min-free-bytes . 0)
              (export-options . ((preset . "ultrafast"))))))

(define (send method url cookie body . headers)
  (parameterize ((log-port (open-output-string))
                 (password-pbkdf2-iterations 1000))
    (app (make-http-request method url
                            (append (if cookie
                                        (list (cons "Cookie" cookie))
                                        '())
                                    headers)
                            body))))

(define (body r)
  (let ((b (http-response-body r)))
    (if (bytevector? b) (utf8->string b) (or b ""))))

(define form '("Content-Type" . "application/x-www-form-urlencoded"))
(define json '("Content-Type" . "application/json"))

(define cookie
  (car (string-split-char (http-response-header
                           (send "POST" "/login" #f
                                 "username=uli&password=uli+password" form)
                           "Set-Cookie")
                          #\;)))

(define pid
  (substring (http-response-header (send "POST" "/projects" cookie "" form)
                                   "Location")
             3
             14))

(define (record-take! with-cam)
  (let* ((r (send "POST"
                  (string-append "/api/projects/" pid "/takes")
                  cookie
                  (if with-cam
                      "{\"screenMime\":\"video/webm;codecs=vp8,opus\",\"camMime\":\"video/webm;codecs=vp8\"}"
                      "{\"screenMime\":\"video/webm;codecs=vp8,opus\",\"camMime\":null}")
                  json))
         (tid (substring (body r) 7 18))
         (base (string-append "/api/projects/" pid "/takes/" tid)))
    (define (upload track file)
      (let loop ((cs (chunks (file-bytes file) 4))
                 (seq 0))
        (when (pair? cs)
          (send "PUT"
                (string-append base "/" track "?seq=" (number->string seq))
                cookie
                (car cs))
          ;; Every chunk twice, as after a lost response: must not matter.
          (send "PUT"
                (string-append base "/" track "?seq=" (number->string seq))
                cookie
                (car cs))
          (loop (cdr cs) (+ seq 1)))))
    (upload "screen" screen)
    (when with-cam (upload "cam" cam))
    ;; The webcam recorder started 0.5 s after the screen recorder.
    (send "POST"
          (string-append base "/finish")
          cookie
          "{\"screenStart\":1000000,\"camStart\":1000500}"
          json)
    tid))

(define t1 (record-take! #t))
(define t2 (record-take! #f))

(test-group "processing"
  (let ((t (take-ref tmp pid t1)))
    (test-equal 'ready (alist-ref t 'status))
    (test-equal 1280 (alist-ref t 'width))
    (test-assert (alist-ref t 'has-audio))
    (test-assert (< 4.9 (alist-ref t 'duration) 5.1))
    (test-equal 0.5 (alist-ref t 'cam-offset))
    (test-equal "screen.webm" (alist-ref (alist-ref t 'screen) 'file))
    (test-equal "cam.webm" (alist-ref (alist-ref t 'cam) 'file))
    (test-assert "raw parts removed"
                 (not (file-exists? (take-file tmp pid t1 "screen.part"))))
    (test-assert (file-exists? (take-file tmp pid t1 "thumb.jpg"))))
  (test-equal 'ready (alist-ref (take-ref tmp pid t2) 'status))
  (test-equal 200
              (http-response-status (send
                                     "GET"
                                     (string-append "/p/" pid "/takes/" t1
                                                    "/screen.webm")
                                     cookie
                                     #f)))
  (test-equal 404
              (http-response-status (send
                                     "GET"
                                     (string-append "/p/" pid "/takes/" t1
                                                    "/take.scm")
                                     cookie
                                     #f))))

(test-group "export"
  (let
      ((r (send "POST"
                (string-append "/p/" pid "/export")
                cookie
                "webcam-on=1&corner=bottom-right&size=0.3"
                form)))
    (test-equal (string-append "/p/" pid "?m=export-queued")
                (http-response-header r "Location")))
  (let ((e (alist-ref (project-ref tmp pid) 'export)))
    (test-equal 'ready (alist-ref e 'status))
    (test-assert (< 9.9 (alist-ref e 'duration) 10.2)))
  (let* ((out (project-file tmp pid "export.mp4"))
         (info (probe-media out)))
    (test-assert (< 9.9 (alist-ref info 'duration) 10.3))
    (test-equal 1280 (alist-ref info 'width))
    (test-equal 720 (alist-ref info 'height))
    (test-assert (alist-ref info 'has-audio))
    ;; The webcam (a bright test pattern, 30% wide) sits bottom right in the
    ;; first take only: compare the corner's brightness at 2 s and 7 s.
    (let
        ((corner-luma (lambda (at)
                        (let
                            ((r (run-program/capture
                                 (list "ffmpeg" "-v" "error" "-ss"
                                       (number->string at)
                                       "-i"
                                       out
                                       "-frames:v"
                                       "1"
                                       "-vf"
                                       "crop=200:100:1040:580,scale=1:1,format=gray"
                                       "-f"
                                       "rawvideo"
                                       "-"))))
                          (and r
                               (> (string-length (cadr r)) 0)
                               (char->integer (string-ref (cadr r) 0)))))))
      (test-assert "webcam overlay differs from the screen in the corner"
                   (not (equal? (corner-luma 2) (corner-luma 7))))))
  (let
      ((r (send "GET"
                (string-append "/p/" pid "/export.mp4?download=1")
                cookie
                #f)))
    (test-equal 200 (http-response-status r))
    (test-assert (string-contains-from (http-response-header r
                                                             "Content-Disposition")
                                       "Aufnahme vom"
                                       0))))

(test-group "editor previews"
  (test-assert (file-exists? (take-file tmp pid t1 "strip.jpg")))
  (test-assert (file-exists? (take-file tmp pid t1 "wave.png")))
  (test-equal 4 (alist-ref (take-ref tmp pid t1) 'strip))
  (let ((info (probe-media (take-file tmp pid t1 "strip.jpg"))))
    (test-equal "4 frames of 160 px" 640 (alist-ref info 'width)))
  (test-equal 200
              (http-response-status (send "GET"
                                          (string-append "/p/" pid "/edit")
                                          cookie
                                          #f)))
  (test-equal 200
              (http-response-status (send
                                     "GET"
                                     (string-append "/p/" pid "/takes/" t1
                                                    "/strip.jpg")
                                     cookie
                                     #f))))

(test-group "cut list and export"
  (let* ((edit (string-append "/api/projects/" pid "/edit"))
         (r (send "PUT" edit cookie
                  (string-append "{\"segments\":[{\"take\":\"" t2
                                 "\",\"start\":0.5,\"end\":2.5},"
                                 "{\"take\":\"" t1
                                 "\",\"start\":1,\"end\":2.5}],"
                                 "\"webcam\":{\"on\":false,\"corner\":\"top-left\",\"size\":0.3},"
                                 "\"loudnorm\":true,\"knownTakes\":[\"" t1
                                 "\",\"" t2 "\"]}")
                  json)))
    (test-equal 200 (http-response-status r))
    (let ((p (project-ref tmp pid)))
      (test-equal 2 (length (alist-ref p 'segments)))
      (test-equal t2 (alist-ref (car (alist-ref p 'segments)) 'take))
      (test-equal #t (alist-ref p 'loudnorm))
      (test-equal #f (alist-ref (alist-ref p 'webcam) 'on))
      (test-equal 'top-left (alist-ref (alist-ref p 'webcam) 'corner)))
    (test-assert (string-contains-from (body (send "GET" edit cookie #f))
                                       "\"loudnorm\":true"
                                       0))
    (test-equal 400
                (http-response-status (send "PUT" edit cookie
                                            "{\"webcam\":{}}" json))))
  (send "POST" (string-append "/p/" pid "/export") cookie "" form)
  (let ((e (alist-ref (project-ref tmp pid) 'export)))
    (test-equal 'ready (alist-ref e 'status))
    (test-assert "2 s + 1.5 s" (< 3.4 (alist-ref e 'duration) 3.6)))
  (let ((info (probe-media (project-file tmp pid "export.mp4"))))
    (test-assert (< 3.4 (alist-ref info 'duration) 3.7))
    (test-assert (alist-ref info 'has-audio)))
  (test-group "a take recorded after editing is appended to the cut list"
    (let ((t3 (record-take! #f)))
      (let ((segs (alist-ref (project-ref tmp pid) 'segments)))
        (test-equal 3 (length segs))
        (test-equal t3 (alist-ref (list-ref segs 2) 'take))))))

(rm tmp 'recursive 'force)
(test-end "aufnahme media")
