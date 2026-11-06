;; (app auth) — sign-in, sessions and password changes.
;;
;; Passwords are hashed with (scm crypto password): PBKDF2-SHA256 with a
;; random salt, verified in constant time. A sign-in with an unknown username
;; does the same hashing work as one with a wrong password, so response
;; times do not reveal which usernames exist. Hashes made with fewer
;; iterations than the current default are upgraded at the next sign-in.
;;
;; Sessions are signed cookies (token-sign), no server-side store. The
;; cookie carries the username and a stamp derived from the password hash,
;; and every request looks the user up again: deleting or disabling a user
;; or changing a password ends that user's other sessions.
;;
;; The cookie is SameSite=Lax rather than Strict, so a link to a video
;; opened from a mail or chat app arrives signed in. Lax cookies are still
;; never sent with a cross-site POST; (app web) additionally refuses
;; state-changing requests whose Origin is another site.

(define-library (app auth)
  (import (scheme base)
          (scm crypto)
          (scm crypto password)
          (scm crypto token)
          (scm net http cookies)
          (scm net http request)
          (app users))
  (export auth-secret
          change-password!
          end-session
          make-auth
          min-password-length
          session-user
          sign-in
          start-session)
  (begin

    (define min-password-length 8)

    (define-record-type auth
      (%make-auth cookie secret max-age path secure? users-file)
      auth?
      (cookie auth-cookie)
      (secret auth-secret)
      (max-age auth-max-age)
      (path auth-path)
      (secure? auth-secure?)
      (users-file auth-users-file))

    (define (make-auth cookie-name secret max-age path secure? users-file)
      "Syntax: (make-auth cookie-name secret max-age path secure? users-file)
Library: (app auth)
Description: The session settings tied to the user store. secret is a
  base64 key or a list of them (newest first); it is checked here."
      (token-secrets secret)
      (%make-auth cookie-name secret max-age path secure? users-file))

    ;; Changes whenever the password (and so the hash) changes.
    (define (password-stamp hash)
      (base64-encode (bytevector-copy (sha256-hash (string->utf8 hash))
                                      0
                                      12)))

    (define (sign-in auth username password)
      "Syntax: (sign-in auth username password)
Library: (app auth)
Description: The user entry if password is right for username and the
  user is not disabled, else #f. Upgrades an outdated password hash."
      (let* ((path (auth-users-file auth))
             (user (find-user path username)))
        (cond ((not (password-verify password (and user (user-hash user))))
               #f)
              ((user-disabled? user) #f)
              ((password-needs-rehash? (user-hash user))
               (update-user! path username 'hash (password-hash password))
               (find-user path username))
              (else user))))

    (define (session-user auth req)
      "Syntax: (session-user auth req)
Library: (app auth)
Description: The signed-in user's entry, or #f if there is no valid session
  or the user was deleted, disabled or changed the password since."
      (let* ((cookies (parse-cookie-header
                       (http-request-header req "Cookie")))
             (tok (cookie-ref cookies (auth-cookie auth)))
             (s (and tok
                     (token-verify (auth-secret auth)
                                   tok
                                   '((purpose . "session"))))))
        (and (pair? s)
             (let* ((name (assq 'user s))
                    (stamp (assq 'stamp s))
                    (user (and name
                               (string? (cdr name))
                               (find-user (auth-users-file auth)
                                          (cdr name)))))
               (and user stamp
                    (not (user-disabled? user))
                    (equal? (cdr stamp) (password-stamp (user-hash user)))
                    user)))))

    (define (cookie-flags auth)
      (if (auth-secure? auth) '(lax) '(lax no-secure)))

    (define (start-session auth user)
      "Syntax: (start-session auth user)
Library: (app auth)
Description: The Set-Cookie header pair that signs user in."
      (cons "Set-Cookie"
            (apply format-set-cookie
                   (auth-cookie auth)
                   (token-sign (auth-secret auth)
                               `((user . ,(user-name user))
                                 (stamp . ,(password-stamp (user-hash user))))
                               (auth-max-age auth)
                               '((purpose . "session")))
                   (auth-max-age auth)
                   (auth-path auth)
                   (cookie-flags auth))))

    (define (end-session auth)
      "Syntax: (end-session auth)
Library: (app auth)
Description: The Set-Cookie header pair that signs out."
      (cons "Set-Cookie"
            (apply format-set-cookie
                   (auth-cookie auth)
                   ""
                   0
                   (auth-path auth)
                   (cookie-flags auth))))

    (define (change-password! auth user new-password)
      "Syntax: (change-password! auth user new-password)
Library: (app auth)
Description: Stores a new hash for user and returns the updated entry.
  Other sessions of the user end; start a new one for this browser."
      (update-user! (auth-users-file auth)
                    (user-name user)
                    'hash
                    (password-hash new-password))
      (find-user (auth-users-file auth) (user-name user)))))
