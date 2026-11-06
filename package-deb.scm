;; Builds dist/aufnahme_<version>_all.deb (see deploy/debian/README.Debian):
;;
;;   scm package-deb.scm [--out DIR] [--dabscm DIR | --jar SCM.JAR] [--allow-dirty]
;;
;; The package bundles the scm.jar of the dabscm checkout next to this repo
;; (../dabscm; build it first with `scm build-scm-java.scm`). The version
;; comes from the last commit; uncommitted changes need --allow-dirty.
;; Installing it pulls in ffmpeg (to process and export recordings).

(import (scheme base)
        (scheme process-context)
        (scm deb webapp))

(webapp-deb-main '((pkg . "aufnahme")
                   (maintainer . "Damian Brunold <damian.brunold@bluewin.ch>")
                   (summary .
                    "aufnahme screencast recorder (dabscm web application)")
                   (description
                    "Records the screen, a window or a browser tab together with the"
                    "microphone and an optional webcam, right in the browser. Recordings"
                    "are edited with a simple cut list, exported as MP4 and can be"
                    "published to filme. All data lives in /var/lib/aufnahme.")
                   (trees "src" "static")
                   (bin-exclude "dev-server.scm")
                   (data-subdirs "projects" "site")
                   (config-settings ("data-dir" . "\"/var/lib/aufnahme\"")
                                    ("secure-cookies?" . "#t"))
                   (setup-hints
                    "create the first administrator: sudo -u aufnahme aufnahme-admin user add <name> admin,"
                    "then put a TLS reverse proxy in front, or set secure-cookies? #f and open"
                    "http://localhost:8073 on this machine (see /usr/share/doc/aufnahme/)")
                   (depends "ffmpeg")
                   (services "aufnahme.service")
                   (timers "aufnahme-health.timer")
                   (default-port . 8073))
                 (cdr (command-line)))
