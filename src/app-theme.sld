;; (app theme) — per-installation branding: data-dir/site/settings.scm.
;;
;; The settings name the site, choose the colours for the light and the
;; dark scheme, the default scheme (auto follows the OS), and optionally a
;; logo and favicon stored next to settings.scm. GET /theme.css renders
;; the colours as CSS custom properties; static/css/app.css uses only
;; those properties, so the strict CSP (style-src 'self') stays intact.

(define-library (app theme)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scm fs)
          (app store))
  (export color-keys
          contrast-ratio
          contrast-warnings
          default-settings
          load-settings
          logo-kinds
          save-settings!
          settings-dir
          settings-version
          theme-css
          valid-color?)
  (begin

    ;; key, German label (for the settings page)
    (define color-keys
      '((bg . "Hintergrund")
        (surface . "Flächen (Karten)")
        (fg . "Text")
        (muted . "Text, gedämpft")
        (accent . "Akzent (Links, Buttons)")
        (on-accent . "Text auf Akzent")
        (border . "Rahmen")))

    (define default-settings
      '((site-name . "Aufnahme")
        (tagline . "")
        (default-mode . auto)
        (light (bg . "#ffffff")
               (surface . "#f6f7f9")
               (fg . "#1a1a1a")
               (muted . "#5f6368")
               (accent . "#2563eb")
               (on-accent . "#ffffff")
               (border . "#e2e4e8"))
        (dark (bg . "#14161b")
              (surface . "#1d2027")
              (fg . "#e8e8e8")
              (muted . "#9aa0aa")
              (accent . "#6ea8fe")
              (on-accent . "#0f172a")
              (border . "#2c2f36"))
        (logo . #f)
        (favicon . #f)))

    ;; file extension -> content type, for logo and favicon uploads
    (define logo-kinds
      '(("png" . "image/png")
        ("webp" . "image/webp")
        ("svg" . "image/svg+xml")))

    (define (settings-dir data-dir) (string-append data-dir "/site"))
    (define (settings-path data-dir)
      (string-append (settings-dir data-dir) "/settings.scm"))

    (define (merge-colors defaults stored)
      (map (lambda (p)
             (let ((s (and (list? stored) (assq (car p) stored))))
               (if (and s (valid-color? (cdr s))) s p)))
           defaults))

    (define (load-settings data-dir . initial-name)
      "Syntax: (load-settings data-dir [initial-site-name])
Library: (app theme)
Description: The stored settings, completed from the defaults (so a
  missing or partial file still yields every key)."
      (let
          ((stored (guard (e (#t '()))
                     (read-datum (settings-path data-dir) '()))))
        (map (lambda (p)
               (let ((s (and (list? stored) (assq (car p) stored))))
                 (case (car p)
                   ((light dark)
                    (cons (car p) (merge-colors (cdr p) (and s (cdr s)))))
                   ((site-name)
                    (cond (s s)
                          ((and (pair? initial-name)
                                (string? (car initial-name)))
                           (cons 'site-name (car initial-name)))
                          (else p)))
                   (else (or s p)))))
             default-settings)))

    (define (save-settings! data-dir settings)
      "Syntax: (save-settings! data-dir settings)
Library: (app theme)
Description: Stores settings atomically."
      (write-datum! (settings-path data-dir) settings))

    (define (settings-version data-dir)
      "Syntax: (settings-version data-dir)
Library: (app theme)
Description: A string that changes whenever the settings change (for
  cache-busting /theme.css and the logo)."
      (let ((p (settings-path data-dir)))
        (if (file-exists? p)
            (number->string (file-modification-timestamp p))
            "0")))

    (define (hex-digit? c)
      (or (char-numeric? c)
          (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))

    (define (valid-color? s)
      "Syntax: (valid-color? s)
Library: (app theme)
Description: True if s is a #rrggbb colour."
      (and (string? s)
           (= (string-length s) 7)
           (char=? (string-ref s 0) #\#)
           (let loop ((i 1))
             (or (= i 7)
                 (and (hex-digit? (string-ref s i)) (loop (+ i 1)))))))

    (define (channel s i)
      (let ((v (/ (string->number (substring s i (+ i 2)) 16) 255.)))
        (if (<= v 0.03928) (/ v 12.92) (expt (/ (+ v 0.055) 1.055) 2.4))))

    (define (luminance s)
      (+ (* 0.2126 (channel s 1))
         (* 0.7152 (channel s 3))
         (* 0.0722 (channel s 5))))

    (define (contrast-ratio a b)
      "Syntax: (contrast-ratio color-a color-b)
Library: (app theme)
Description: The WCAG contrast ratio of two #rrggbb colours (1 to 21)."
      (let ((la (luminance a))
            (lb (luminance b)))
        (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))

    (define (contrast-warnings settings)
      "Syntax: (contrast-warnings settings)
Library: (app theme)
Description: German messages for colour pairs below WCAG AA (4.5:1 for
  text, 3:1 for muted text)."
      (let loop ((schemes '((light . "Hell") (dark . "Dunkel"))) (acc '()))
        (if (null? schemes)
            (reverse acc)
            (let* ((c (alist-ref settings (caar schemes)))
                   (check (lambda (fg bg min what acc)
                            (let
                                ((r (contrast-ratio (alist-ref c fg)
                                                    (alist-ref c bg))))
                              (if (< r min)
                                  (cons
                                   (string-append
                                    (cdar schemes)
                                    ": "
                                    what
                                    " hat zu wenig Kontrast ("
                                    (number->string (/ (round (* r 10)) 10))
                                    ":1)")
                                   acc)
                                  acc)))))
              (loop (cdr schemes)
                    (check 'muted
                           'bg
                           3
                           "Gedämpfter Text"
                           (check 'on-accent
                                  'accent
                                  4.5
                                  "Text auf Akzent"
                                  (check
                                   'accent
                                   'bg
                                   3
                                   "Akzent"
                                   (check
                                    'fg
                                    'surface
                                    4.5
                                    "Text auf Flächen"
                                    (check 'fg 'bg 4.5 "Text" acc))))))))))

    (define (theme-css settings)
      "Syntax: (theme-css settings)
Library: (app theme)
Description: The CSS that defines the colour properties for settings."
      (let ((light (alist-ref settings 'light))
            (dark (alist-ref settings 'dark))
            (mode (alist-ref settings 'default-mode 'auto)))
        (string-append "/* generated from the site settings */\n"
                       ":root {\n  color-scheme: "
                       (case mode
                         ((light) "light")
                         ((dark) "dark")
                         (else "light dark"))
                       ";\n"
                       (apply string-append
                              (map (lambda (k)
                                     (string-append "  --"
                                                    (symbol->string (car k))
                                                    ": light-dark("
                                                    (alist-ref light (car k))
                                                    ", "
                                                    (alist-ref dark (car k))
                                                    ");\n"))
                                   color-keys))
                       "}\n"
                       ":root[data-theme=\"light\"] { color-scheme: light; }\n"
                       ":root[data-theme=\"dark\"] { color-scheme: dark; }\n")))))
