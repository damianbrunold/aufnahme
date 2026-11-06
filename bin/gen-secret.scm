;; Usage: scm bin/gen-secret.scm
;;
;; Prints a fresh base64 secret (32 random bytes) for session-secret.

(import (scheme base)
        (scheme write)
        (scm crypto))

(display (base64-encode (random-bytes 32)))
(newline)
