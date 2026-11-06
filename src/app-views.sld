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
          edit-page
          error-page
          format-duration
          home-page
          login-page
          not-found-page
          notice-text
          profile-page
          project-page
          record-page)
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
        ("no-title" error . "Bitte einen Titel angeben.")
        ("renamed" success . "Titel gespeichert.")
        ("take-deleted" success . "Aufnahme gelöscht.")
        ("take-rescued" success .
         "Die unterbrochene Aufnahme wird verarbeitet.")
        ("export-queued" success . "Der Export wurde gestartet.")
        ("no-takes" error . "Es gibt noch keine fertigen Aufnahmen.")
        ("project-deleted" success . "Projekt gelöscht.")))

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

    (define (support-card)
      `(div (@ (class "card"))
            (h2 "Kann dieser Browser aufnehmen?")
            (table (@ (class "data support js-support"))
                   (tbody ,@(map (lambda (r)
                                   `(tr (@ (data-check ,(car r)))
                                        (th (@ (scope "row")) ,(cdr r))
                                        (td (@ (class "js-result")) "…")
                                        (td (@ (class "hint js-detail")) "")))
                                 support-rows)))
            (p (@ (class "hint"))
               "Am besten geeignet ist ein aktueller Chrome oder Edge. "
               "Firefox und Safari nehmen Bildschirm, Mikrofon und Webcam auf, "
               "aber keinen Ton von Tabs oder vom System.")))

    (define (format-size n)
      (cond ((>= n 1073741824)
             (string-append (number->string (/ (round (/ n 107374182.4)) 10))
                            " GB"))
            ((>= n 1048576)
             (string-append (number->string (exact (round (/ n 1048576))))
                            " MB"))
            (else (string-append (number->string (exact (round (/ n 1024))))
                                 " KB"))))

    (define (project-url p . more)
      (url (apply string-append "/p/" (alist-ref p 'id) more)))

    (define (export-label e)
      (case (alist-ref e 'status 'none)
        ((queued) "Export wartet …")
        ((running) "Export läuft …")
        ((ready) "Exportiert")
        ((failed) "Export fehlgeschlagen")
        (else "Nicht exportiert")))

    ;; projects: list of (project . takes).
    (define (home-page ctx projects show-owner?)
      (page ctx "Projekte"
            'home
            `(section (div (@ (class "page-head"))
                           (h1 "Projekte")
                           (form (@ (method "post")
                                    (action ,(url "/projects")))
                                 (button (@ (type "submit") (class "primary"))
                                         "Neue Aufnahme")))
                      ,(if (null? projects)
                           '(p (@ (class "empty")) "Noch keine Projekte.")
                           `(div (@ (class "table-wrap"))
                                 (table
                                  (@ (class "data"))
                                  (thead
                                   (tr (th "Titel")
                                       (th (@ (class "num")) "Aufnahmen")
                                       (th (@ (class "num")) "Dauer")
                                       (th "Export")
                                       ,@(if show-owner? '((th "Von")) '())
                                       (th "Geändert")))
                                  (tbody
                                   ,@(map
                                      (lambda (pt)
                                        (let
                                            ((p (car pt))
                                             (takes (cdr pt)))
                                          `(tr
                                            (td
                                             (a (@ (href ,(project-url p)))
                                                ,(alist-ref p 'title "")))
                                            (td
                                             (@ (class "num"))
                                             ,(number->string (length takes)))
                                            (td
                                             (@ (class "num"))
                                             ,(format-duration
                                               (let loop ((ts takes) (acc 0))
                                                 (if
                                                  (null? ts)
                                                  acc
                                                  (loop
                                                   (cdr ts)
                                                   (+
                                                    acc
                                                    (alist-ref (car ts)
                                                               'duration
                                                               0)))))))
                                            (td
                                             ,(export-label
                                               (alist-ref p 'export '())))
                                            ,@(if
                                               show-owner?
                                               `((td
                                                  ,(alist-ref p 'owner "")))
                                               '())
                                            (td
                                             ,(format-date
                                               (alist-ref p 'updated 0))))))
                                      projects)))))
                      ,(support-card))
            "js/support.js"))

    (define (take-status-label t)
      (case (alist-ref t 'status)
        ((recording) "Aufnahme läuft oder wurde unterbrochen")
        ((processing) "Wird verarbeitet …")
        ((failed) (string-append "Fehler: " (or (alist-ref t 'error) "")))
        (else "")))

    (define (take-item p t n)
      (let* ((tid (alist-ref t 'id))
             (ready? (eq? (alist-ref t 'status) 'ready))
             (screen (alist-ref t 'screen))
             (cam (alist-ref t 'cam)))
        `(li (@ (class "card take")
                (data-take ,tid)
                (data-status ,(symbol->string (alist-ref t 'status))))
             (div (@ (class "take-media"))
                  ,(if ready?
                       `(video (@ (controls #t)
                                  (preload "none")
                                  (poster
                                   ,(project-url p "/takes/" tid
                                                 "/thumb.jpg"))
                                  (src ,(project-url
                                         p
                                         "/takes/"
                                         tid
                                         "/"
                                         (alist-ref screen 'file)))))
                       `(div (@ (class "take-placeholder"))
                             ,(take-status-label t))))
             (div (@ (class "take-info"))
                  (h3 ,(string-append "Aufnahme " (number->string n)))
                  (p (@ (class "meta-line"))
                     ,(format-duration (alist-ref t 'duration 0))
                     ,@(if ready?
                           (list " · "
                                 (number->string (alist-ref t 'width 0))
                                 "×"
                                 (number->string (alist-ref t 'height 0))
                                 (if (alist-ref t 'has-audio #f)
                                     " · mit Ton"
                                     " · ohne Ton")
                                 (if (and cam (alist-ref cam 'file))
                                     " · mit Webcam"
                                     ""))
                           '()))
                  (div (@ (class "button-row"))
                       ,(if (and ready? cam (alist-ref cam 'file))
                            `(a (@ (class "button")
                                   (href
                                    ,(project-url p "/takes/" tid "/"
                                                  (alist-ref cam 'file))))
                                "Webcam ansehen")
                            "")
                       ,(if (eq? (alist-ref t 'status) 'recording)
                            `(form (@ (method "post")
                                      (class "inline")
                                      (action
                                       ,(project-url p "/takes/" tid
                                                     "/finish")))
                                   (button (@ (type "submit"))
                                           "Unterbrochene Aufnahme retten"))
                            "")
                       (form (@ (method "post")
                                (class "inline js-confirm")
                                (data-confirm "Diese Aufnahme löschen?")
                                (action
                                 ,(project-url p "/takes/" tid "/delete")))
                             (button (@ (type "submit") (class "danger"))
                                     "Löschen")))))))

    (define corner-labels
      '((bottom-right . "unten rechts")
        (bottom-left . "unten links")
        (top-right . "oben rechts")
        (top-left . "oben links")))

    (define (project-page ctx p takes msg)
      (let* ((e (alist-ref p 'export '()))
             (cam (alist-ref p 'webcam '()))
             (status (alist-ref e 'status 'none)))
        (page ctx
              (alist-ref p 'title "Projekt")
              'project
              `(section (@ (class "project js-project")
                           (data-project ,(alist-ref p 'id))
                           (data-api ,(url
                                       (string-append "/api/projects/"
                                                      (alist-ref p 'id))))
                           (data-export ,(symbol->string status)))
                        (p (a (@ (href ,(url "/"))) "‹ Alle Projekte"))
                        ,(notice msg)
                        (form (@ (method "post")
                                 (action ,(project-url p "/title"))
                                 (class "title-form"))
                              (label (@ (class "visually-hidden")
                                        (for "title"))
                                     "Titel")
                              (input (@ (id "title")
                                        (name "title")
                                        (required #t)
                                        (maxlength "200")
                                        (value ,(alist-ref p 'title ""))))
                              (button (@ (type "submit")) "Umbenennen"))
                        (div (@ (class "page-head"))
                             (h2 "Aufnahmen")
                             (a (@ (class "button primary")
                                   (href ,(project-url p "/record")))
                                ,(if (null? takes)
                                     "Aufnahme starten"
                                     "Weitere Aufnahme")))
                        ,(if (null? takes)
                             '(p (@ (class "empty")) "Noch keine Aufnahmen.")
                             `(ol (@ (class "takes"))
                                  ,@(let loop ((ts takes) (n 1))
                                      (if
                                       (null? ts)
                                       '()
                                       (cons (take-item p (car ts) n)
                                             (loop (cdr ts) (+ n 1)))))))
                        (div (@ (class "card"))
                             (div (@ (class "page-head"))
                                  (h2 "Schnitt und Export")
                                  ,(if (null? takes)
                                       ""
                                       `(a
                                         (@ (class "button")
                                            (href ,(project-url p "/edit")))
                                         "Schneiden und Webcam einstellen")))
                             (p (@ (class "hint"))
                                ,(if (alist-ref p 'segments #f)
                                     "Der Export folgt dem Schnitt aus dem Editor."
                                     "Ohne Schnitt werden alle fertigen Aufnahmen der Reihe nach zusammengefügt.")
                                " Ergebnis: ein MP4-Video (H.264/AAC)"
                                ,(if (alist-ref cam 'on #t)
                                     ", mit eingeblendeter Webcam."
                                     "."))
                             (form (@ (method "post")
                                      (action ,(project-url p "/export")))
                                   (button
                                    (@ (type "submit")
                                       (class "primary")
                                       (disabled
                                        ,(or (memq status '(queued running))
                                             (null? takes))))
                                    "Exportieren"))
                             (div (@ (class "export-status"))
                                  (p (@ (class "js-export-label"))
                                     ,(export-label e)
                                     ,(if
                                       (eq? status 'failed)
                                       (string-append
                                        ": "
                                        (or (alist-ref e 'error) ""))
                                       ""))
                                  (progress
                                   (@ (class "js-export-progress")
                                      (max "100")
                                      (value
                                       ,(number->string
                                         (exact
                                          (round
                                           (* 100
                                              (alist-ref e 'progress 0))))))
                                      (hidden
                                       ,(not
                                         (memq status '(queued running)))))))
                             ,(if (eq? status 'ready)
                                  `(div
                                    (@ (class "export-result"))
                                    (video
                                     (@
                                      (controls #t)
                                      (preload "metadata")
                                      (class "export-video")
                                      (src
                                       ,(string-append
                                         (project-url p "/export.mp4")
                                         "?v="
                                         (number->string
                                          (alist-ref e 'finished 0))))))
                                    (p (@ (class "meta-line"))
                                       ,(format-duration
                                         (alist-ref e 'duration 0))
                                       " · "
                                       ,(format-size (alist-ref e 'size 0)))
                                    (a (@
                                        (class "button primary")
                                        (href
                                         ,(string-append
                                           (project-url p "/export.mp4")
                                           "?download=1")))
                                       "Herunterladen"))
                                  ""))
                        (form (@ (method "post")
                                 (action ,(project-url p "/delete"))
                                 (class "card danger-zone js-confirm")
                                 (data-confirm
                                  "Das ganze Projekt mit allen Aufnahmen löschen?"))
                              (h2 "Projekt löschen")
                              (button (@ (type "submit") (class "danger"))
                                      "Projekt löschen")))
              "js/project.js"
              "js/confirm.js")))

    (define (edit-page ctx p)
      (page ctx "Schneiden"
            'edit
            `(section (@ (class "editor js-editor")
                         (data-api ,(url
                                     (string-append "/api/projects/"
                                                    (alist-ref p 'id))))
                         (data-project-url ,(project-url p)))
                      (p (a (@ (href ,(project-url p)))
                            ,(string-append "‹ " (alist-ref p 'title ""))))
                      (h1 "Schneiden")
                      (div (@ (class "editor-grid"))
                           (div (@ (class "stage js-stage"))
                                (video (@ (class "js-main")
                                          (preload "auto")
                                          (playsinline #t)))
                                (video (@ (class "camview js-camview")
                                          (preload "auto")
                                          (muted #t)
                                          (playsinline #t)
                                          (hidden #t))))
                           (div (@ (class "card editor-side"))
                                (h2 "Webcam")
                                (label (@ (class "check"))
                                       (input
                                        (@ (type "checkbox")
                                           (class "js-cam-on")))
                                       "Webcam einblenden")
                                (label "Position"
                                       (select
                                        (@ (class "js-corner"))
                                        ,@(map
                                           (lambda (c)
                                             `(option
                                               (@
                                                (value
                                                 ,(symbol->string (car c))))
                                               ,(cdr c)))
                                           corner-labels)))
                                (label "Grösse"
                                       (input
                                        (@ (type "range")
                                           (class "js-size")
                                           (min "0.1")
                                           (max "0.4")
                                           (step "0.01"))))
                                (h2 "Ton")
                                (label (@ (class "check"))
                                       (input
                                        (@ (type "checkbox")
                                           (class "js-loudnorm")))
                                       "Lautstärke angleichen")
                                (p (@ (class "hint"))
                                   "Gleicht die Lautheit auf einen üblichen Wert an (EBU R128).")
                                (p (@ (class "hint js-save-status")
                                      (aria-live "polite")))))
                      (div (@ (class "transport"))
                           (button (@ (type "button")
                                      (class "primary js-play")
                                      (title "Leertaste"))
                                   "Abspielen")
                           (span (@ (class "time js-time")) "0:00 / 0:00")
                           (button (@ (type "button")
                                      (class "js-split")
                                      (title "S"))
                                   "Teilen")
                           (button (@ (type "button")
                                      (class "js-remove")
                                      (title "Entf"))
                                   "Entfernen")
                           (button (@ (type "button")
                                      (class "js-left")
                                      (title "Abschnitt nach vorne"))
                                   "← Nach vorne")
                           (button (@ (type "button")
                                      (class "js-right")
                                      (title "Abschnitt nach hinten"))
                                   "Nach hinten →")
                           (button (@ (type "button")
                                      (class "js-undo")
                                      (title "Strg+Z"))
                                   "Rückgängig")
                           (button (@ (type "button") (class "js-reset"))
                                   "Alles zurücksetzen"))
                      (div (@ (class "timeline js-timeline")
                              (tabindex "0")
                              (aria-label
                               "Zeitleiste: klicken zum Springen, Abschnitt wählen"))
                           (div (@ (class "segments js-segments")))
                           (div (@ (class "playhead js-playhead"))))
                      (p (@ (class "hint"))
                         "Klicken Sie in die Zeitleiste, um an eine Stelle zu springen und einen Abschnitt "
                         "auszuwählen. „Teilen“ schneidet an der Abspielposition, „Entfernen“ löscht den "
                         "gewählten Abschnitt. Tasten: Leertaste, S, Entf, ←/→ (eine Sekunde), Strg+Z. "
                         "Änderungen werden automatisch gespeichert.")
                      (form (@ (method "post")
                               (action ,(project-url p "/export"))
                               (class "js-export-form"))
                            (button (@ (type "submit") (class "primary"))
                                    "Exportieren")))
            "js/editor.js"))

    (define (record-page ctx p)
      (page ctx "Aufnehmen"
            'record
            `(section (@ (class "recorder js-recorder")
                         (data-project ,(alist-ref p 'id))
                         (data-api ,(url
                                     (string-append "/api/projects/"
                                                    (alist-ref p 'id))))
                         (data-project-url ,(project-url p)))
                      (p (a (@ (href ,(project-url p)))
                            ,(string-append "‹ " (alist-ref p 'title ""))))
                      (h1 "Aufnehmen")
                      (p (@ (class "notice error js-unsupported")
                            (hidden #t)))
                      (div (@ (class "record-grid"))
                           (div (@ (class "card"))
                                (h2 "Bildschirm")
                                (video (@ (class "preview js-screen-preview")
                                          (muted #t)
                                          (autoplay #t)
                                          (playsinline #t)))
                                (div (@ (class "button-row"))
                                     (button
                                      (@ (type "button")
                                         (class "js-pick-screen"))
                                      "Bildschirm, Fenster oder Tab wählen …"))
                                (label (@
                                        (class "check js-display-audio-row"))
                                       (input
                                        (@ (type "checkbox")
                                           (class "js-display-audio")
                                           (checked #t)))
                                       "Ton des Tabs bzw. Systems mit aufnehmen")
                                (label "Qualität"
                                       (select
                                        (@ (class "js-quality"))
                                        (option (@ (value "1080"))
                                                "bis 1080p")
                                        (option (@ (value "720"))
                                                "bis 720p"))))
                           (div (@ (class "card"))
                                (h2 "Mikrofon und Webcam")
                                (div (@ (class "button-row"))
                                     (button
                                      (@ (type "button")
                                         (class "js-allow-devices"))
                                      "Mikrofon und Kamera freigeben"))
                                (label "Mikrofon"
                                       (select
                                        (@ (class "js-mic"))
                                        (option (@ (value ""))
                                                "Kein Mikrofon")))
                                (meter (@ (class "level js-level")
                                          (min "0")
                                          (max "1")
                                          (low "0.6")
                                          (high "0.85")
                                          (optimum "0.3")
                                          (value "0")))
                                (label "Webcam"
                                       (select
                                        (@ (class "js-cam"))
                                        (option (@ (value ""))
                                                "Keine Webcam")))
                                (video (@ (class "preview cam js-cam-preview")
                                          (muted #t)
                                          (autoplay #t)
                                          (playsinline #t)
                                          (hidden #t)))))
                      (div (@ (class "card record-bar"))
                           (span (@ (class "timer js-timer")) "0:00")
                           (button (@ (type "button")
                                      (class "primary record js-start")
                                      (disabled #t))
                                   "Aufnahme starten")
                           (button (@ (type "button")
                                      (class "js-pause")
                                      (hidden #t))
                                   "Pause")
                           (button (@ (type "button")
                                      (class "danger js-stop")
                                      (hidden #t))
                                   "Stopp")
                           (span (@ (class "hint js-status")
                                    (aria-live "polite"))
                                 "Wählen Sie zuerst, was aufgenommen werden soll.")))
            "js/record.js"))

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
