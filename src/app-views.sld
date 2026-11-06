;; (app views) — HTML rendering. All dynamic values flow through the
;; auto-escaping (scm html builder), so user data can never inject markup.
;; Only fixed strings (the SVG icons) and the output of markdown->html
;; (which escapes text and URLs itself) are emitted raw. No inline scripts
;; or style attributes: the Content-Security-Policy forbids them.
;;
;; Every page gets a ctx alist: (user . entry-or-#f) (settings . alist)
;; (theme-v . string). The UI text is German; messages shown after a
;; redirect are looked up by code in `notices`.

(define-library (app views)
  (import (scheme base)
          (scm datetime)
          (scm html builder)
          (app site)
          (app store)
          (app theme)
          (app users))
  (export admin-site-page
          admin-users-page
          error-page
          format-duration
          home-page
          login-page
          not-found-page
          notice-text
          profile-page)
  (begin

    ;; --- messages ------------------------------------------------------

    (define notices
      '(("saved" success . "Gespeichert.")
        ("pw" success . "Passwort geändert.")
        ("user-added" success . "Benutzer angelegt.")
        ("user-saved" success . "Benutzer gespeichert.")
        ("user-deleted" success . "Benutzer gelöscht.")
        ("bad-user" error .
         "Ungültiger Benutzername (Buchstaben, Ziffern, . _ -).")
        ("user-exists" error . "Diesen Benutzer gibt es schon.")
        ("short-pw" error .
         "Das Passwort muss mindestens 8 Zeichen lang sein.")
        ("self" error . "Das eigene Konto kann so nicht geändert werden.")
        ("site-saved" success . "Darstellung gespeichert.")
        ("site-reset" success . "Darstellung zurückgesetzt.")
        ("logo-saved" success . "Bild gespeichert.")
        ("logo-bad" error . "Nur PNG, WebP oder SVG bis 1 MB.")
        ("bad-color" error . "Ungültige Farbe.")
        ("no-title" error . "Bitte einen Titel angeben.")))

    (define (notice-text code)
      "Syntax: (notice-text code)
Library: (app views)
Description: (kind . text) for a notice code, or #f."
      (let ((p (and code (assoc code notices)))) (and p (cdr p))))

    (define (notice msg)
      (if msg
          `(p (@ (class ,(string-append "notice " (symbol->string (car msg))))
                 (role "status"))
              ,(cdr msg))
          ""))

    ;; --- formatting ------------------------------------------------------

    (define (two n)
      (if (< n 10) (string-append "0" (number->string n)) (number->string n)))

    (define (format-duration secs)
      "Syntax: (format-duration seconds)
Library: (app views)
Description: \"m:ss\" or \"h:mm:ss\"."
      (let* ((s (exact (round (if (number? secs) secs 0))))
             (h (quotient s 3600))
             (m (quotient (remainder s 3600) 60))
             (r (remainder s 60)))
        (if (> h 0)
            (string-append (number->string h) ":" (two m) ":" (two r))
            (string-append (number->string m) ":" (two r)))))

    (define (format-date secs)
      (let ((iso (format-iso8601 (if (number? secs) secs 0))))
        ;; "YYYY-MM-DDT..." -> "DD.MM.YYYY"
        (string-append (substring iso 8 10)
                       "."
                       (substring iso 5 7)
                       "."
                       (substring iso 0 4))))

    (define role-labels '((user . "Aufnehmen") (admin . "Administration")))

    (define (label-of table key)
      (alist-ref table key (if (symbol? key) (symbol->string key) "")))

    ;; --- icons -----------------------------------------------------------

    (define theme-icons
      (raw (string-append
            "<svg class=\"i-sun\" viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><circle cx=\"12\" cy=\"12\" r=\"4\"/><path d=\"M12 2v2M12 20v2M2 12h2M20 12h2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M19.1 4.9l-1.4 1.4M6.3 17.7l-1.4 1.4\"/></svg>"
            "<svg class=\"i-moon\" viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><path d=\"M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z\"/></svg>")))

    ;; --- layout ----------------------------------------------------------

    (define (ctx-user ctx) (alist-ref ctx 'user))
    (define (ctx-settings ctx) (alist-ref ctx 'settings))
    (define (site-name ctx)
      (alist-ref (ctx-settings ctx) 'site-name "Aufnahme"))

    (define (site-asset ctx kind)
      (string-append (url (string-append "/site/" kind))
                     "?v="
                     (alist-ref ctx 'theme-v "0")))

    (define (nav-link href label current here)
      `(a (@ (href ,(url href))
             (aria-current ,(if (equal? current here) "page" #f)))
          ,label))

    (define (page ctx title here body . scripts)
      (let* ((user (ctx-user ctx))
             (settings (ctx-settings ctx))
             (mode (alist-ref settings 'default-mode 'auto)))
        (string-append (html->string (html5
                                      `(@
                                        (lang "de")
                                        (data-theme-default
                                         ,(if (eq? mode 'auto)
                                              #f
                                              (symbol->string mode))))
                                      `(head
                                        (meta (@ (charset "utf-8")))
                                        (meta
                                         (@
                                          (name "viewport")
                                          (content
                                           "width=device-width, initial-scale=1")))
                                        (title
                                         ,(if
                                           title
                                           (string-append title " · "
                                                          (site-name ctx))
                                           (site-name ctx)))
                                        (link
                                         (@
                                          (rel "icon")
                                          (href
                                           ,(if (alist-ref settings 'favicon)
                                                (site-asset ctx "favicon")
                                                (asset "favicon.svg")))))
                                        (link
                                         (@ (rel "stylesheet")
                                            (href ,(asset "css/app.css"))))
                                        (link
                                         (@
                                          (rel "stylesheet")
                                          (href
                                           ,(string-append
                                             (url "/theme.css")
                                             "?v="
                                             (alist-ref ctx 'theme-v "0")))))
                                        (script
                                         (@
                                          (src ,(asset "js/theme-init.js"))))
                                        (script
                                         (@
                                          (src ,(asset "js/theme-toggle.js"))
                                          (defer #t)))
                                        ,@(map
                                           (lambda (s)
                                             `(script
                                               (@ (src ,(asset s))
                                                  (type "module"))))
                                           scripts))
                                      `(body
                                        (a (@ (class "skip") (href "#main"))
                                           "Zum Inhalt")
                                        (header
                                         (@ (class "app-header"))
                                         (a
                                          (@ (class "brand")
                                             (href ,(url "/")))
                                          ,(if
                                            (alist-ref settings 'logo)
                                            `(img
                                              (@
                                               (src ,(site-asset ctx "logo"))
                                               (alt "")
                                               (class "logo")))
                                            "")
                                          (span (@ (class "brand-name"))
                                                ,(site-name ctx)))
                                         (nav
                                          (@ (class "nav")
                                             (aria-label "Hauptnavigation"))
                                          ,@(if
                                             user
                                             (list
                                              (nav-link "/" "Projekte" here
                                                        'home))
                                             '())
                                          ,@(if
                                             (has-role? user 'admin)
                                             (list
                                              (nav-link "/admin/users"
                                                        "Benutzer"
                                                        here
                                                        'users)
                                              (nav-link "/admin/site"
                                                        "Darstellung"
                                                        here
                                                        'site))
                                             '())
                                          ,(if
                                            user
                                            `(a
                                              (@ (href ,(url "/profile"))
                                                 (class "who"))
                                              ,(user-name user))
                                            (nav-link "/login" "Anmelden" here
                                                      'login))
                                          (button
                                           (@
                                            (class "icon-btn js-theme-toggle")
                                            (type "button")
                                            (aria-label
                                             "Hell/Dunkel umschalten"))
                                           ,theme-icons)))
                                        (main (@ (id "main")) ,body)
                                        ,(let
                                             ((tag
                                               (alist-ref settings
                                                          'tagline
                                                          "")))
                                           (if
                                            (string=? tag "")
                                            ""
                                            `(footer (@ (class "app-footer"))
                                                     ,tag))))))
                       "\n")))

    ;; --- sign in / profile -----------------------------------------------

    (define (login-page ctx error? next)
      (page ctx "Anmelden"
            'login
            `(section (@ (class "card narrow"))
                      (h1 "Anmelden")
                      ,(notice (and error?
                                    '(error .
                                      "Benutzername oder Passwort ist falsch.")))
                      (form (@ (method "post") (action ,(url "/login")))
                            (input (@ (type "hidden")
                                      (name "next")
                                      (value ,(or next ""))))
                            (label "Benutzername"
                                   (input
                                    (@ (name "username")
                                       (autocomplete "username")
                                       (autofocus #t)
                                       (required #t))))
                            (label "Passwort"
                                   (input
                                    (@ (name "password")
                                       (type "password")
                                       (autocomplete "current-password")
                                       (required #t))))
                            (button (@ (type "submit") (class "primary"))
                                    "Anmelden")))))

    (define (profile-page ctx msg)
      (let ((user (ctx-user ctx)))
        (page ctx "Profil"
              'profile
              `(section (@ (class "card narrow"))
                        (h1 "Profil")
                        ,(notice msg)
                        (p "Angemeldet als "
                           (strong ,(user-name user))
                           " ("
                           ,(label-of role-labels (user-role user))
                           ").")
                        (h2 "Passwort ändern")
                        (form (@ (method "post")
                                 (action ,(url "/profile/password")))
                              (label "Aktuelles Passwort"
                                     (input
                                      (@ (name "current")
                                         (type "password")
                                         (autocomplete "current-password")
                                         (required #t))))
                              (label "Neues Passwort"
                                     (input
                                      (@ (name "new")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (label "Neues Passwort bestätigen"
                                     (input
                                      (@ (name "confirm")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (button (@ (type "submit") (class "primary"))
                                      "Passwort ändern"))
                        (form (@ (method "post")
                                 (action ,(url "/logout"))
                                 (class "logout-form"))
                              (button (@ (type "submit")) "Abmelden"))))))

    ;; --- the video grid ----------------------------------------------------

    ;; --- watch page ------------------------------------------------------

    ;; --- editor pages ----------------------------------------------------

    ;; --- admin pages -----------------------------------------------------

    (define (admin-users-page ctx users msg)
      (let ((me (user-name (ctx-user ctx))))
        (page ctx "Benutzer"
              'users
              `(section (h1 "Benutzer")
                        ,(notice msg)
                        (div (@ (class "table-wrap"))
                             (table (@ (class "data"))
                                    (thead
                                     (tr (th "Name")
                                         (th "Rolle")
                                         (th "Status")
                                         (th "Seit")
                                         (th "")))
                                    (tbody
                                     ,@(map
                                        (lambda (u)
                                          (let*
                                              ((name (user-name u))
                                               (action
                                                (url
                                                 (string-append
                                                  "/admin/users/" name))))
                                            `(tr
                                              (td ,name)
                                              (td
                                               ,(if
                                                 (string=? name me)
                                                 (label-of role-labels
                                                           (user-role u))
                                                 `(form
                                                   (@ (method "post")
                                                      (action ,action)
                                                      (class "inline"))
                                                   (input
                                                    (@ (type "hidden")
                                                       (name "op")
                                                       (value "role")))
                                                   (select
                                                    (@ (name "role")
                                                       (aria-label "Rolle"))
                                                    ,@(map
                                                       (lambda (r)
                                                         `(option
                                                           (@
                                                            (value
                                                             ,(symbol->string
                                                               (car r)))
                                                            (selected
                                                             ,(eq?
                                                               (car r)
                                                               (user-role
                                                                u))))
                                                           ,(cdr r)))
                                                       role-labels))
                                                   (button (@ (type "submit"))
                                                           "Setzen"))))
                                              (td
                                               ,(if (user-disabled? u)
                                                    "gesperrt"
                                                    "aktiv"))
                                              (td
                                               ,(format-date
                                                 (user-created u)))
                                              (td
                                               ,(if
                                                 (string=? name me)
                                                 "(Sie)"
                                                 `(div
                                                   (@ (class "button-row"))
                                                   (form
                                                    (@ (method "post")
                                                       (action ,action)
                                                       (class "inline"))
                                                    (input
                                                     (@
                                                      (type "hidden")
                                                      (name "op")
                                                      (value
                                                       ,(if (user-disabled? u)
                                                            "enable"
                                                            "disable"))))
                                                    (button
                                                     (@ (type "submit"))
                                                     ,(if (user-disabled? u)
                                                          "Entsperren"
                                                          "Sperren")))
                                                   (form
                                                    (@ (method "post")
                                                       (action ,action)
                                                       (class "inline"))
                                                    (input
                                                     (@ (type "hidden")
                                                        (name "op")
                                                        (value "password")))
                                                    (input
                                                     (@
                                                      (type "password")
                                                      (name "password")
                                                      (placeholder
                                                       "Neues Passwort")
                                                      (minlength "8")
                                                      (autocomplete
                                                       "new-password")
                                                      (required #t)
                                                      (aria-label
                                                       "Neues Passwort")))
                                                    (button
                                                     (@ (type "submit"))
                                                     "Setzen"))
                                                   (form
                                                    (@
                                                     (method "post")
                                                     (action ,action)
                                                     (class
                                                      "inline js-confirm")
                                                     (data-confirm
                                                      ,(string-append name
                                                                      " löschen?")))
                                                    (input
                                                     (@ (type "hidden")
                                                        (name "op")
                                                        (value "delete")))
                                                    (button
                                                     (@ (type "submit")
                                                        (class "danger"))
                                                     "Löschen"))))))))
                                        users))))
                        (form (@ (method "post")
                                 (action ,(url "/admin/users"))
                                 (class "card narrow"))
                              (h2 "Neuer Benutzer")
                              (label "Benutzername"
                                     (input
                                      (@ (name "username")
                                         (required #t)
                                         (autocomplete "off"))))
                              (label "Passwort"
                                     (input
                                      (@ (name "password")
                                         (type "password")
                                         (minlength "8")
                                         (autocomplete "new-password")
                                         (required #t))))
                              (label "Rolle"
                                     (select
                                      (@ (name "role"))
                                      ,@(map
                                         (lambda (r)
                                           `(option
                                             (@
                                              (value
                                               ,(symbol->string (car r))))
                                             ,(cdr r)))
                                         role-labels)))
                              (p (@ (class "hint"))
                                 "Benutzer nehmen auf und bearbeiten ihre Projekte; "
                                 "Administration verwaltet zusätzlich Benutzer und Darstellung.")
                              (button (@ (type "submit") (class "primary"))
                                      "Anlegen")))
              "js/confirm.js")))

    (define (color-inputs scheme colors)
      `(fieldset (legend ,(if (eq? scheme 'light)
                              "Helles Farbschema"
                              "Dunkles Farbschema"))
                 (div (@ (class "color-grid"))
                      ,@(map (lambda (k)
                               (let
                                   ((name
                                     (string-append
                                      (symbol->string scheme)
                                      "-"
                                      (symbol->string (car k)))))
                                 `(label
                                   (@ (class "color"))
                                   (input
                                    (@ (type "color")
                                       (name ,name)
                                       (class "js-color")
                                       (data-scheme ,(symbol->string scheme))
                                       (data-key ,(symbol->string (car k)))
                                       (value ,(alist-ref colors (car k)))))
                                   (span ,(cdr k)))))
                             color-keys))))

    (define (admin-site-page ctx msg warnings)
      (let ((s (ctx-settings ctx)))
        (page ctx "Darstellung"
              'site
              `(section (h1 "Darstellung")
                        ,(notice msg)
                        ,@(map (lambda (w)
                                 `(p (@ (class "notice warning")) ,w))
                               warnings)
                        (form (@ (method "post")
                                 (action ,(url "/admin/site"))
                                 (class "card js-site-form"))
                              (label "Name der Seite"
                                     (input
                                      (@
                                       (name "site-name")
                                       (required #t)
                                       (maxlength "80")
                                       (value ,(alist-ref s 'site-name "")))))
                              (label "Fusszeile (optional)"
                                     (input
                                      (@ (name "tagline")
                                         (maxlength "200")
                                         (value ,(alist-ref s 'tagline "")))))
                              (fieldset
                               (legend "Standard-Farbschema")
                               ,@(map (lambda (m)
                                        `(label
                                          (@ (class "radio"))
                                          (input
                                           (@
                                            (type "radio")
                                            (name "default-mode")
                                            (value ,(symbol->string (car m)))
                                            (checked
                                             ,(eq?
                                               (car m)
                                               (alist-ref s
                                                          'default-mode
                                                          'auto)))))
                                          ,(cdr m)))
                                      '((auto . "Wie das Betriebssystem")
                                        (light . "Hell")
                                        (dark . "Dunkel"))))
                              ,(color-inputs 'light (alist-ref s 'light))
                              ,(color-inputs 'dark (alist-ref s 'dark))
                              (div (@ (class "preview-pair"))
                                   ,@(map
                                      (lambda (scheme)
                                        `(div
                                          (@ (class "preview js-preview")
                                             (data-scheme ,scheme))
                                          (div
                                           (@ (class "pv-surface"))
                                           (strong "Beispieltitel")
                                           (p (@ (class "pv-muted"))
                                              "06.11.2026 · 12:34")
                                           (p
                                            "Text mit "
                                            (span (@ (class "pv-link"))
                                                  "Link"))
                                           (span (@ (class "pv-button"))
                                                 "Button"))))
                                      '("light" "dark")))
                              (button (@ (type "submit") (class "primary"))
                                      "Speichern"))
                        (div (@ (class "card"))
                             (h2 "Logo und Favicon")
                             (p (@ (class "hint"))
                                "PNG, WebP oder SVG, höchstens 1 MB. Das Logo erscheint links in der Kopfzeile.")
                             ,@(map (lambda (k)
                                      `(div
                                        (@ (class "logo-row"))
                                        (strong
                                         ,(if (equal? k "logo")
                                              "Logo"
                                              "Favicon"))
                                        ,(if
                                          (alist-ref s (string->symbol k))
                                          `(img
                                            (@ (src ,(site-asset ctx k))
                                               (alt "")
                                               (class "logo-preview")))
                                          '(span (@ (class "hint")) "keines"))
                                        (form
                                         (@
                                          (method "post")
                                          (enctype "multipart/form-data")
                                          (action
                                           ,(url
                                             (string-append "/admin/site/"
                                                            k)))
                                          (class "inline js-image-upload"))
                                         (input
                                          (@
                                           (type "file")
                                           (name "file")
                                           (accept
                                            "image/png,image/webp,image/svg+xml")
                                           (aria-label "Bild wählen")))
                                         (button (@ (type "submit"))
                                                 "Hochladen"))
                                        ,(if
                                          (alist-ref s (string->symbol k))
                                          `(form
                                            (@
                                             (method "post")
                                             (action
                                              ,(url
                                                (string-append "/admin/site/"
                                                               k "/delete")))
                                             (class "inline"))
                                            (button (@ (type "submit"))
                                                    "Entfernen"))
                                          "")))
                                    '("logo" "favicon")))
                        (form (@ (method "post")
                                 (action ,(url "/admin/site/reset"))
                                 (class "card js-confirm")
                                 (data-confirm
                                  "Alle Farben und Namen auf die Standardwerte zurücksetzen?"))
                              (button (@ (type "submit"))
                                      "Auf Standard zurücksetzen")))
              "js/site-settings.js"
              "js/confirm.js")))

    ;; --- home ------------------------------------------------------------------

    ;; The browser check is filled in by static/js/support.js: each row
    ;; names a capability and gets "ja"/"nein" plus a hint.
    (define support-rows
      '(("secure" . "Sichere Verbindung (HTTPS oder localhost)")
        ("display" . "Bildschirm, Fenster oder Tab aufnehmen")
        ("devices" . "Mikrofon und Webcam")
        ("recorder" . "Aufnahmeformat")
        ("display-audio" . "Ton von Tab oder System")
        ("pip" . "Schwebendes Steuerfenster")))

    (define (home-page ctx)
      (page ctx "Projekte"
            'home
            `(section (h1 "Projekte")
                      (p (@ (class "empty")) "Noch keine Projekte.")
                      (div (@ (class "card"))
                           (h2 "Kann dieser Browser aufnehmen?")
                           (table (@ (class "data support js-support"))
                                  (tbody
                                   ,@(map
                                      (lambda (r)
                                        `(tr
                                          (@ (data-check ,(car r)))
                                          (th (@ (scope "row")) ,(cdr r))
                                          (td (@ (class "js-result")) "…")
                                          (td (@ (class "hint js-detail"))
                                              "")))
                                      support-rows)))
                           (p (@ (class "hint"))
                              "Am besten geeignet ist ein aktueller Chrome oder Edge. "
                              "Firefox und Safari nehmen Bildschirm, Mikrofon und Webcam auf, "
                              "aber keinen Ton von Tabs oder vom System.")))
            "js/support.js"))

    ;; --- errors --------------------------------------------------------------

    (define (not-found-page ctx)
      (page ctx "Nicht gefunden" #f
            `(section (h1 "Nicht gefunden")
                      (p "Diese Seite gibt es nicht (mehr).")
                      (p (a (@ (href ,(url "/"))) "Zur Übersicht")))))

    (define (error-page ctx)
      (page ctx "Fehler" #f
            '(section (h1 "Etwas ist schiefgelaufen")
                      (p "Der Fehler wurde protokolliert."))))))
