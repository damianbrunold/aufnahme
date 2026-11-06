;; (app timeline) — turns an edit into an ffmpeg command line. Pure: no
;; files are touched here, so the generated arguments are unit-tested.
;;
;; A segment is a piece of one take to keep:
;;   ((screen . path) (cam . path-or-#f) (cam-offset . s) (has-audio . bool)
;;    (start . s) (end . s) (width . px) (height . px))
;; cam-offset is how much later the webcam recording started than the
;; screen recording (negative: earlier). Times are in seconds of the take.
;;
;; Every segment becomes one screen input (seeked with -ss/-to) and, if it
;; has a webcam, one webcam input. Each segment is scaled and padded to the
;; output size, gets the webcam overlaid in a corner, is padded or trimmed
;; to its exact length (video and audio), and all segments are joined with
;; concat. Optionally the joined audio is loudness-normalized (EBU R128).

(define-library (app timeline)
  (import (scheme base))
  (export clean-segments
          default-segments
          export-arguments
          output-size
          segment-length
          segments-length
          strip-count)
  (begin

    (define (ref al key default)
      (let ((p (assq key al))) (if p (cdr p) default)))

    (define (num->string x)
      ;; Millisecond precision, no exponent notation.
      (let* ((ms (exact (round (* 1000 x))))
             (sign (if (< ms 0) "-" ""))
             (ms (abs ms))
             (s (quotient ms 1000))
             (f (remainder ms 1000)))
        (string-append sign
                       (number->string s)
                       "."
                       (cond ((< f 10) "00")
                             ((< f 100) "0")
                             (else ""))
                       (number->string f))))

    (define (even n) (* 2 (exact (round (/ n 2)))))

    ;; --- the cut list ----------------------------------------------------
    ;;
    ;; A project's cut list is a list of ((take . tid) (start . s) (end . e)):
    ;; the pieces to keep, in playing order. takes below is a list of
    ;; (tid . duration) for the project's ready takes, in recording order.

    (define (default-segments takes)
      "Syntax: (default-segments takes)
Library: (app timeline)
Description: The cut list that keeps every take whole, in order. takes is
  a list of (take-id . duration)."
      (map (lambda (t) `((take . ,(car t)) (start . 0) (end . ,(cdr t))))
           takes))

    ;; Segments shorter than this are dropped (a stray click while cutting).
    (define min-segment 0.05)

    (define (clean-segments segs takes)
      "Syntax: (clean-segments segments takes)
Library: (app timeline)
Description: segments with every entry checked against takes (a list of
  (take-id . duration)): unknown takes are dropped, times are clamped to
  the take and rounded to milliseconds, pieces shorter than 50 ms are
  dropped. #f (no cut list saved) gives the default cut list."
      (if (not (list? segs))
          (default-segments takes)
          (let loop ((ss segs) (acc '()))
            (if (null? ss)
                (reverse acc)
                (let* ((seg (car ss))
                       (tid (and (list? seg) (ref seg 'take #f)))
                       (t (and (string? tid) (assoc tid takes)))
                       (start (and t (ref seg 'start #f)))
                       (end (and t (ref seg 'end #f))))
                  (if (and t (real? start) (real? end))
                      (let* ((ms (lambda (x)
                                   (/ (round (* 1000 (exact x))) 1000)))
                             (a (ms (max 0 (min start (cdr t)))))
                             (b (ms (max 0 (min end (cdr t))))))
                        (loop (cdr ss)
                              (if (< (- b a) min-segment)
                                  acc
                                  (cons
                                   `((take . ,tid)
                                     (start . ,(inexact a))
                                     (end . ,(inexact b)))
                                   acc))))
                      (loop (cdr ss) acc)))))))

    (define (strip-count duration)
      "Syntax: (strip-count duration)
Library: (app timeline)
Description: How many frames the film strip of a take of duration
  seconds shows: one per 5 s, at least 4, at most 120."
      (max 4 (min 120 (exact (ceiling (/ duration 5))))))

    (define (segment-length seg)
      "Syntax: (segment-length segment)
Library: (app timeline)
Description: The length in seconds of a segment (end - start)."
      (max 0 (- (ref seg 'end 0) (ref seg 'start 0))))

    (define (segments-length segs)
      "Syntax: (segments-length segments)
Library: (app timeline)
Description: The total length in seconds of segments."
      (let loop ((ss segs) (acc 0))
        (if (null? ss)
            acc
            (loop (cdr ss) (+ acc (segment-length (car ss)))))))

    (define (output-size segs max-width)
      "Syntax: (output-size segments max-width)
Library: (app timeline)
Description: (width . height) of the export: the first segment's screen
  size, scaled down to max-width, both even."
      (let* ((w (ref (car segs) 'width 1280))
             (h (ref (car segs) 'height 720))
             (w (if (> w 0) w 1280))
             (h (if (> h 0) h 720)))
        (if (> w max-width)
            (cons (even max-width) (even (* h (/ max-width w))))
            (cons (even w) (even h)))))

    (define (overlay-position corner margin)
      (let ((m (number->string margin)))
        (case corner
          ((top-left) (string-append m ":" m))
          ((top-right) (string-append "W-w-" m ":" m))
          ((bottom-left) (string-append m ":H-h-" m))
          (else (string-append "W-w-" m ":H-h-" m)))))

    (define (export-arguments segs webcam options output progress)
      "Syntax: (export-arguments segments webcam options output progress)
Library: (app timeline)
Description: The ffmpeg arguments (without the program name) that render
  segments into the MP4 file output, writing -progress lines to progress.
  webcam is an alist (on . bool) (corner . top-left|top-right|bottom-left|
  bottom-right) (size . fraction of the width) (margin . px). options is
  an alist: max-width (1920), fps (30), preset (\"veryfast\"), crf (23),
  audio-bitrate (\"160k\"), loudnorm (#f)."
      (let* ((max-width (ref options 'max-width 1920))
             (fps (number->string (ref options 'fps 30)))
             (size (output-size segs max-width))
             (W (number->string (car size)))
             (H (number->string (cdr size)))
             (cam-on (ref webcam 'on #t))
             (cam-w (number->string (even
                                     (* (car size) (ref webcam 'size 0.22)))))
             (pos (overlay-position (ref webcam 'corner 'bottom-right)
                                    (ref webcam 'margin 24))))
        (let loop
            ((ss segs)
             (k 0)
             (input 0)
             (inputs '())
             (filters '())
             (labels '()))
          (if (pair? ss)
              (let* ((seg (car ss))
                     (len (num->string (segment-length seg)))
                     (start (ref seg 'start 0))
                     (end (ref seg 'end 0))
                     (cam (and cam-on (ref seg 'cam #f)))
                     (off (ref seg 'cam-offset 0))
                     (ks (number->string k))
                     (si (number->string input))
                     (ci (number->string (+ input 1)))
                     (screen-in (list "-ss"
                                      (num->string start)
                                      "-to"
                                      (num->string end)
                                      "-i"
                                      (ref seg 'screen "")))
                     (cam-in (if cam (list "-i" cam) '()))
                     (base (string-append
                            "["
                            si
                            ":v]setpts=PTS-STARTPTS,fps="
                            fps
                            ",scale="
                            W
                            ":"
                            H
                            ":force_original_aspect_ratio=decrease"
                            ",pad="
                            W
                            ":"
                            H
                            ":(ow-iw)/2:(oh-ih)/2:color=black,setsar=1"
                            (if cam
                                (string-append "[b" ks "]")
                                (string-append
                                 ",format=yuv420p,tpad=stop=-1:stop_mode=clone,trim=duration="
                                 len ",setpts=PTS-STARTPTS[v" ks "]"))))
                     ;; The webcam frame shown at take time t was recorded
                     ;; at webcam time t - off.
                     (cam-from (- start off))
                     (cam-to (- end off))
                     (cam-chain (if cam
                                    (list
                                     (string-append
                                      "["
                                      ci
                                      ":v]setpts=PTS-STARTPTS"
                                      ",trim=start="
                                      (num->string (max 0 cam-from))
                                      ":end="
                                      (num->string (max 0 cam-to))
                                      ",setpts=PTS-STARTPTS+"
                                      (num->string (max 0 (- cam-from)))
                                      "/TB"
                                      ",fps="
                                      fps
                                      ",scale="
                                      cam-w
                                      ":-2[c"
                                      ks
                                      "]")
                                     (string-append "[b" ks "][c" ks
                                                    "]overlay=" pos
                                                    ":eof_action=pass"
                                                    ",format=yuv420p,tpad=stop=-1:stop_mode=clone"
                                                    ",trim=duration=" len
                                                    ",setpts=PTS-STARTPTS[v"
                                                    ks "]"))
                                    '()))
                     (audio (if (ref seg 'has-audio #f)
                                (string-append "[" si
                                               ":a]asetpts=PTS-STARTPTS,aresample=48000"
                                               ",aformat=sample_fmts=fltp:channel_layouts=stereo"
                                               ",apad,atrim=duration=" len
                                               ",asetpts=PTS-STARTPTS[a" ks
                                               "]")
                                (string-append
                                 "anullsrc=r=48000:cl=stereo,atrim=duration="
                                 len
                                 ",aformat=sample_fmts=fltp:channel_layouts=stereo[a"
                                 ks "]"))))
                (loop (cdr ss)
                      (+ k 1)
                      (+ input (if cam 2 1))
                      (append inputs screen-in cam-in)
                      (append filters (list base) cam-chain (list audio))
                      (append labels
                              (list (string-append "[v" ks "][a" ks "]")))))
              (let* ((n (number->string k))
                     (concat (string-append (apply string-append labels)
                                            "concat=n="
                                            n
                                            ":v=1:a=1[v][a0]"))
                     (audio-out (if (ref options 'loudnorm #f)
                                    "[a0]loudnorm=I=-16:TP=-1.5:LRA=11,aresample=48000[a]"
                                    "[a0]anull[a]"))
                     (graph (let join ((fs
                                        (append filters
                                                (list concat audio-out)))
                                       (acc ""))
                              (if (null? fs)
                                  acc
                                  (join
                                   (cdr fs)
                                   (if (string=? acc "")
                                       (car fs)
                                       (string-append acc ";" (car fs))))))))
                (append (list "-nostdin" "-y" "-v" "error")
                        inputs
                        (list "-filter_complex" graph "-map" "[v]" "-map"
                              "[a]"
                              "-c:v"
                              "libx264"
                              "-preset"
                              (ref options 'preset "veryfast")
                              "-crf"
                              (number->string (ref options 'crf 23))
                              "-pix_fmt"
                              "yuv420p"
                              "-c:a"
                              "aac"
                              "-b:a"
                              (ref options 'audio-bitrate "160k")
                              "-movflags"
                              "+faststart"
                              "-progress"
                              progress
                              "-nostats"
                              output)))))))))
