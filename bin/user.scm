;; Usage: scm bin/user.scm [--config path] add <username> [user|admin]
;;        scm bin/user.scm [--config path] passwd <username>
;;        scm bin/user.scm [--config path] role <username> user|admin
;;        scm bin/user.scm [--config path] disable|enable|delete <username>
;;        scm bin/user.scm [--config path] list
;;
;; Manages the user store (data-dir/users.scm). There is no sign-up page:
;; the first admin is created here, further users also in the browser
;; (Benutzer). Passwords are prompted for twice and read without echo.
;; Changing, disabling or deleting a user ends their sessions.

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scm fs)
        (scm module)
        (scm terminal))

(define root
  (directory-name (directory-name (absolute-path (car (command-line))))))
(module-search-path! (cons (string-append root "/src") (module-search-path)))

(import (scm crypto password)
        (app auth)
        (app cli)
        (app store)
        (app users))

(define usage
  "usage: scm bin/user.scm [--config path] add <name> [role] | passwd <name> | role <name> <role> | disable|enable|delete <name> | list")

(define parsed (cli-config root (cdr (command-line))))
(define data-dir (cli-data-dir root (car parsed)))
(define users-file (string-append data-dir "/users.scm"))
(define args (cdr parsed))

(define (ask-new-password)
  (let* ((a (read-password "Password: "))
         (b (if (eof-object? a) a (read-password "Confirm:  "))))
    (cond ((or (eof-object? a) (eof-object? b)) (cli-fail "aborted"))
          ((not (string=? a b)) (cli-fail "passwords do not match"))
          ((< (string-length a) min-password-length)
           (cli-fail "password must be at least 8 characters"))
          (else a))))

(define (arg n) (if (> (length args) n) (list-ref args n) (cli-fail usage)))

(define (existing name)
  (or (find-user users-file name)
      (cli-fail (string-append "no such user: " name))))

(define (parse-role s)
  (let ((r (string->symbol s)))
    (if (valid-role? r) r (cli-fail "role must be user or admin"))))

(define (locked thunk) (with-data-lock data-dir thunk))

(when (null? args) (cli-fail usage))
(let ((cmd (car args)))
  (cond ((string=? cmd "add")
         (let ((name (arg 1))
               (role (if (> (length args) 2) (parse-role (arg 2)) 'user)))
           (unless (valid-username? name)
             (cli-fail
              "invalid username (letters, digits, . _ -; at most 64)"))
           (when (find-user users-file name)
             (cli-fail (string-append "user exists: " name " (use passwd)")))
           (let ((hash (password-hash (ask-new-password))))
             (locked (lambda () (save-user! users-file name hash role))))
           (display (string-append "added " name " ("
                                   (symbol->string role)
                                   ")\n"))))
        ((string=? cmd "passwd")
         (let ((name (arg 1)))
           (existing name)
           (let ((hash (password-hash (ask-new-password))))
             (locked (lambda () (update-user! users-file name 'hash hash))))
           (display (string-append "password changed for " name "\n"))))
        ((string=? cmd "role")
         (let ((name (arg 1))
               (role (parse-role (arg 2))))
           (existing name)
           (locked (lambda () (update-user! users-file name 'role role)))
           (display (string-append name " is now "
                                   (symbol->string role)
                                   "\n"))))
        ((member cmd '("disable" "enable"))
         (let ((name (arg 1)))
           (existing name)
           (locked (lambda ()
                     (update-user! users-file name
                                   'disabled
                                   (string=? cmd "disable"))))
           (display (string-append cmd "d " name "\n"))))
        ((string=? cmd "delete")
         (let ((name (arg 1)))
           (existing name)
           (locked (lambda () (delete-user! users-file name)))
           (display (string-append "deleted " name "\n"))))
        ((string=? cmd "list")
         (for-each (lambda (u)
                     (display (string-append
                               (user-name u)
                               "\t"
                               (symbol->string (user-role u))
                               (if (user-disabled? u) "\tdisabled" "")))
                     (newline))
                   (load-users users-file)))
        (else (cli-fail usage))))
