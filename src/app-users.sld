;; (app users) — the user store, data-dir/users.scm.
;;
;; users.scm holds one datum: a list of user alists
;;   ((name . "alice") (hash . "pbkdf2$...") (role . user)
;;    (created . 1793971758) (disabled . #f))
;; Roles are ordered: user < admin; a user has the rights of
;; their role and every role below it. The file is written atomically by
;; the app and by the CLI (bin/user.scm), which takes the data-dir lock.
;;
;; This module is storage only; passwords and sessions live in (app auth).

(define-library (app users)
  (import (scheme base)
          (scheme char)
          (app store))
  (export delete-user!
          find-user
          has-role?
          load-users
          role-names
          save-user!
          update-user!
          user-created
          user-disabled?
          user-hash
          user-name
          user-role
          valid-role?
          valid-username?)
  (begin

    (define role-names '(user admin))

    (define (user-name u) (alist-ref u 'name))
    (define (user-hash u) (alist-ref u 'hash))
    (define (user-role u) (alist-ref u 'role 'user))
    (define (user-created u) (alist-ref u 'created 0))
    (define (user-disabled? u) (alist-ref u 'disabled #f))

    (define (valid-role? r) (and (memq r role-names) #t))

    (define (role-rank r)
      (let loop ((rs role-names) (i 0))
        (cond ((null? rs) -1)
              ((eq? (car rs) r) i)
              (else (loop (cdr rs) (+ i 1))))))

    (define (has-role? user role)
      "Syntax: (has-role? user role)
Library: (app users)
Description: True if user (an entry, or #f) is enabled and has role or a
  higher one: (has-role? u 'user) holds for users and admins."
      (and user
           (not (user-disabled? user))
           (>= (role-rank (user-role user)) (role-rank role))))

    (define (load-users path)
      "Syntax: (load-users path)
Library: (app users)
Description: The list of user entries in path, or '() if there is none."
      (let ((d (read-datum path '()))) (if (list? d) d '())))

    (define (find-user path name)
      "Syntax: (find-user path name)
Library: (app users)
Description: The entry for name, or #f."
      (let loop ((us (load-users path)))
        (cond ((null? us) #f)
              ((equal? (user-name (car us)) name) (car us))
              (else (loop (cdr us))))))

    (define (update-users! path proc)
      (write-datum! path (proc (load-users path))))

    (define (save-user! path name hash role)
      "Syntax: (save-user! path name hash role)
Library: (app users)
Description: Adds user name with the given password hash and role."
      (update-users! path
                     (lambda (us)
                       (append (filter-out name us)
                               (list `((name . ,name)
                                       (hash . ,hash)
                                       (role . ,role)
                                       (created . ,(now-seconds))
                                       (disabled . #f)))))))

    (define (update-user! path name key value)
      "Syntax: (update-user! path name key value)
Library: (app users)
Description: Sets one field (hash, role, disabled) of name's entry."
      (update-users! path
                     (lambda (us)
                       (map (lambda (u)
                              (if (equal? (user-name u) name)
                                  (alist-set u key value)
                                  u))
                            us))))

    (define (filter-out name us)
      (let loop ((us us))
        (cond ((null? us) '())
              ((equal? (user-name (car us)) name) (loop (cdr us)))
              (else (cons (car us) (loop (cdr us)))))))

    (define (delete-user! path name)
      "Syntax: (delete-user! path name)
Library: (app users)
Description: Removes name's entry, if there is one."
      (update-users! path (lambda (us) (filter-out name us))))

    (define (valid-username? s)
      "Syntax: (valid-username? s)
Library: (app users)
Description: True if s is 1-64 characters of letters, digits and . _ -"
      (and (string? s)
           (<= 1 (string-length s) 64)
           (let loop ((i 0))
             (or (= i (string-length s))
                 (let ((c (string-ref s i)))
                   (and (or (char-alphabetic? c)
                            (char-numeric? c)
                            (memv c '(#\. #\_ #\-)))
                        (loop (+ i 1))))))))))
