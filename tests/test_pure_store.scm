;; Tests for the data layer without HTTP or ffmpeg: store, library index,
;; search, uploads, users, the access rules and the theme.
;; Run: scm tests/test_pure_store.scm

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

(import (app store)
        (app theme)
        (app users))

(test-runner-factory scm-test-runner)
(test-begin "aufnahme store")

(define tmp (mktempdir '(prefix . "aufnahme-store")))

(test-group "store"
  (let ((p (string-append tmp "/a/b/x.scm")))
    (test-equal 'none (read-datum p 'none))
    (write-datum! p '((a . 1) (b "x" "y")))
    (test-equal '((a . 1) (b "x" "y")) (read-datum p #f))
    (test-assert "no temp file left"
                 (not (file-exists? (string-append p ".tmp")))))
  (test-equal 2 (alist-ref '((a . 1) (b . 2)) 'b))
  (test-equal 'd (alist-ref '((a . 1)) 'z 'd))
  (test-equal '((a . 9) (b . 2)) (alist-set '((a . 1) (b . 2)) 'a 9))
  (test-equal '((a . 1) (c . 3)) (alist-set '((a . 1)) 'c 3))
  (let ((id (new-id)))
    (test-equal 11 (string-length id))
    (test-assert (valid-id? id)))
  (test-assert (not (valid-id? "../etc")))
  (test-assert (not (valid-id? "a/b..")))
  (test-equal 42 (with-data-lock tmp (lambda () 42))))

(define users-file (string-append tmp "/users.scm"))

(test-group "users"
  (save-user! users-file "vera" "h1" 'user)
  (save-user! users-file "ed" "h2" 'user)
  (save-user! users-file "ada" "h3" 'admin)
  (test-equal 3 (length (load-users users-file)))
  (test-equal 'user (user-role (find-user users-file "ed")))
  (test-assert (has-role? (find-user users-file "ada") 'user))
  (test-assert (has-role? (find-user users-file "ada") 'admin))
  (test-assert (not (has-role? (find-user users-file "vera") 'admin)))
  (test-assert (not (has-role? #f 'user)))
  (update-user! users-file "ed" 'disabled #t)
  (test-assert "disabled users have no rights"
               (not (has-role? (find-user users-file "ed") 'user)))
  (delete-user! users-file "ed")
  (test-equal #f (find-user users-file "ed"))
  (test-assert (valid-username? "a.b_c-1"))
  (test-assert (not (valid-username? "a b"))))

(define (filter-warnings ws prefix)
  (let loop ((ws ws))
    (cond ((null? ws) '())
          ((string-contains-from (car ws) prefix 0)
           (cons (car ws) (loop (cdr ws))))
          (else (loop (cdr ws))))))

(test-group "theme"
  (let ((s (load-settings tmp "Technik")))
    (test-equal "initial name from the config" "Technik"
                (alist-ref s 'site-name))
    (test-equal 'auto (alist-ref s 'default-mode))
    (test-equal '() (contrast-warnings s))
    (save-settings! tmp
                    (alist-set s
                               'light
                               (alist-set (alist-ref s 'light)
                                          'fg
                                          "#fefefe")))
    (let ((s2 (load-settings tmp "Technik")))
      (test-equal "#fefefe" (alist-ref (alist-ref s2 'light) 'fg))
      (test-equal 1
                  (length (filter-warnings (contrast-warnings s2)
                                           "Hell: Text hat"))))
    (test-assert (string-contains-from (theme-css s)
                                       "--accent: light-dark(#2563eb, #6ea8fe);"
                                       0)))
  (test-assert "partial file falls back to defaults"
               (begin
                 (write-datum! (string-append tmp "/site/settings.scm")
                               '((light (bg . "bad"))))
                 (equal? "#ffffff"
                         (alist-ref (alist-ref (load-settings tmp) 'light)
                                    'bg))))
  (test-assert (valid-color? "#a0B1c2"))
  (test-assert (not (valid-color? "red")))
  (test-assert (> (contrast-ratio "#000000" "#ffffff") 20.9)))

(rm tmp 'recursive 'force)
(test-end "aufnahme store")
