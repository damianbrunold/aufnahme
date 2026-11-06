;; (app filme) — publishes an export to filme, the companion video portal.
;;
;; Uses filme's resumable upload API with a personal token of the user
;; (created in filme under Profil -> API-Tokens):
;;   POST /api/uploads            {"filename", "size"} -> {"id", "chunkSize"}
;;   PUT  /api/uploads/:id?offset=N   raw chunk          -> {"offset"}
;;   POST /api/uploads/:id/complete   {"title", "description", "tags"}
;;                                                       -> {"video", "editUrl"}
;; The video arrives in filme as a draft. A chunk that fails is retried
;; (after asking filme for the current offset), a few times with a pause.

(define-library (app filme)
  (import (scheme base)
          (scheme char)
          (srfi 18)
          (scm fs)
          (scm json simple)
          (scm net http client)
          (scm net http request)
          (scm net http response)
          (scm random access))
  (export filme-publish!
          normalize-filme-url)
  (begin

    (define (normalize-filme-url s)
      "Syntax: (normalize-filme-url s)
Library: (app filme)
Description: s without trailing slashes, or #f if it is not an http(s)
  URL."
      (and (string? s)
           (let* ((s (let loop ((s s))
                       (if (and (> (string-length s) 0)
                                (char=? (string-ref s (- (string-length s) 1))
                                        #\/))
                           (loop (substring s 0 (- (string-length s) 1)))
                           s)))
                  (l (string-downcase s)))
             (and (or (and (> (string-length l) 8)
                           (string=? (substring l 0 8) "https://"))
                      (and (> (string-length l) 7)
                           (string=? (substring l 0 7) "http://")))
                  s))))

    (define (body-string r)
      (let ((b (http-response-body r)))
        (cond ((bytevector? b) (utf8->string b))
              ((string? b) b)
              (else ""))))

    (define (json-of r)
      (guard (e (#t '()))
        (let ((j (json-parse (body-string r)))) (if (list? j) j '()))))

    (define (call method url token body content-type)
      (http-send (make-http-request method url
                                    (append
                                     (list
                                      (cons "Authorization"
                                            (string-append "Bearer " token))
                                      (cons "Accept" "application/json"))
                                     (if
                                      content-type
                                      (list
                                       (cons "Content-Type" content-type))
                                      '()))
                                    body
                                    120)))

    (define (fail r what)
      (let
          ((msg (let ((e (json-ref (json-of r) "error" #f)))
                  (and (string? e) e))))
        (error (string-append what ": "
                              (case (http-response-status r)
                                ((401)
                                 "das Token ist ungültig oder widerrufen")
                                ((403)
                                 "das Token hat keine Berechtigung zum Hochladen")
                                (else (string-append
                                       "Status "
                                       (number->string
                                        (http-response-status r))
                                       (if msg
                                           (string-append " – " msg)
                                           ""))))))))

    (define (filme-publish! base token path filename info progress)
      "Syntax: (filme-publish! base token path filename info progress)
Library: (app filme)
Description: Uploads the MP4 file path to the filme at base (\"https://
  filme.example.com\") with token, as a new draft video. info is an alist
  of title, description and tags (a string); progress is called with the
  fraction sent (0..1). Returns the alist (video . id) (url . edit page
  URL). Raises with a German message on failure."
      (let* ((size (file-size path))
             (r (guard
                    (e (#t
                        (error (string-append
                                "filme ist nicht erreichbar: "
                                (if (error-object? e)
                                    (error-object-message e)
                                    "")))))
                  (call "POST"
                        (string-append base "/api/uploads")
                        token
                        (json->string `(("filename" . ,filename)
                                        ("size" . ,size)))
                        "application/json")))
             (j (json-of r)))
        (unless (= (http-response-status r) 201)
          (fail r "Upload konnte nicht beginnen"))
        (let ((id (json-ref j "id" #f))
              (chunk (let ((c (json-ref j "chunkSize" 0)))
                       (if (and (integer? c) (> c 0)) c 8388608))))
          (call-with-random-access-file path
            'read
            (lambda (f)
              (let loop ((offset 0) (failures 0))
                (when (< offset size)
                  (let* ((bytes (random-access-file-read
                                 f
                                 offset
                                 (min chunk (- size offset))))
                         (r (guard (e (#t #f))
                              (call "PUT"
                                    (string-append base "/api/uploads/" id
                                                   "?offset="
                                                   (number->string offset))
                                    token
                                    bytes
                                    "application/octet-stream"))))
                    (cond ((and r (= (http-response-status r) 200))
                           (let
                               ((next (json-ref
                                       (json-of r)
                                       "offset"
                                       (+ offset (bytevector-length bytes)))))
                             (progress (/ next size))
                             (loop next 0)))
                          ((and r (= (http-response-status r) 409))
                           (loop (json-ref (json-of r) "offset" offset)
                                 failures))
                          ((and r (< (http-response-status r) 500))
                           (fail r "Upload abgebrochen"))
                          ((< failures 5)
                           ;; Network or server trouble: wait and ask where to go on.
                           (thread-sleep! (* 2 (+ failures 1)))
                           (let
                               ((s (guard (e (#t #f))
                                     (call
                                      "GET"
                                      (string-append base "/api/uploads/" id)
                                      token
                                      #f
                                      #f))))
                             (loop (if (and s
                                            (= (http-response-status s) 200))
                                       (json-ref (json-of s) "offset" offset)
                                       offset)
                                   (+ failures 1))))
                          (else (error
                                 "Upload abgebrochen: filme antwortet nicht mehr"))))))))
          (let
              ((r (call "POST"
                        (string-append base "/api/uploads/" id "/complete")
                        token
                        (json->string `(("title" . ,(cdr (assq 'title info)))
                                        ("description"
                                         .
                                         ,(cdr (assq 'description info)))
                                        ("tags" . ,(cdr (assq 'tags info)))))
                        "application/json")))
            (unless (= (http-response-status r) 200)
              (fail r "Abschluss fehlgeschlagen"))
            (let* ((j (json-of r))
                   (edit (json-ref j "editUrl" "")))
              `((video . ,(json-ref j "video" #f))
                ;; editUrl is a path on filme ("/v/<id>/edit", with its
                ;; prefix); base may carry the same prefix.
                (url . ,(string-append (origin-of base) edit))))))))

    ;; "https://host:port/prefix" -> "https://host:port"
    (define (origin-of base)
      (let ((i (let find ((i 0))
                 (cond ((>= (+ i 3) (string-length base)) #f)
                       ((string=? (substring base i (+ i 3)) "://") i)
                       (else (find (+ i 1)))))))
        (if (not i)
            base
            (let loop ((j (+ i 3)))
              (cond ((= j (string-length base)) base)
                    ((char=? (string-ref base j) #\/) (substring base 0 j))
                    (else (loop (+ j 1))))))))))
