# aufnahme

A screencast recorder that runs in the browser: a small "OBS" for
recording tutorials. It is written in [dabscm](../dabscm) and is the
companion of [filme](../filme), which presents the finished videos.

**Status: phase 1 (skeleton).** It has sign-in, users and roles, theming,
a check of what the current browser can record, and packaging. Recording
comes next. The full plan, in short:

1. **Record:**
   - Capture a screen, window or tab with `getDisplayMedia`, plus the
     microphone and optional tab or system audio (mixed with Web Audio).
   - The webcam is recorded as a separate track.
   - The recording streams to the server in 2-second chunks, so it is
     crash-safe.
   - A project can hold several takes.
2. **Process:** ffprobe reads each take. ffmpeg remuxes it so it can be
   seeked, and makes a film strip and a waveform for the editor.
3. **Edit:**
   - A cut list: split, remove, reorder, undo.
   - Webcam layout: corner, size, or off.
   - Loudness normalization.
4. **Export:** ffmpeg composes the cuts and the webcam overlay into an
   H.264/AAC MP4. You can download it or publish it to filme, where it
   arrives as a draft.

## Where it runs

Browsers only allow screen capture on **HTTPS** or on
**http://localhost**:

- **Central server:** behind a TLS reverse proxy, like filme (see
  `deploy/nginx.conf.example`).
- **Recording PC:** installed locally. Open `http://localhost:8073` and
  set `secure-cookies? #f`.

## Browser support

| | Chrome / Edge | Firefox | Safari |
|---|---|---|---|
| Screen, window, tab | yes | yes | yes |
| Microphone, webcam | yes | yes | yes |
| Tab audio | yes | no | no |
| System audio | Windows, ChromeOS; limited on Linux and macOS | no | no |
| Floating control window (Document PiP) | yes | no (popup instead) | no (popup instead) |

The start page shows these checks for the browser you are using.

## Run (development)

```sh
scm bin/user.scm add alice admin   # prompts for a password; creates config.scm on the first run
scm bin/server.scm                 # http://127.0.0.1:8073
scm bin/dev-server.scm             # the same, restarts when src/ or the config changes
```

Data goes to `var/` (gitignored). `config.example.scm` lists every setting.

## Layout

```
bin/server.scm        load the config, serve the app
bin/dev-server.scm    auto-reloading dev supervisor
bin/user.scm          add / passwd / role / disable / enable / delete / list users
bin/gen-secret.scm    print a new session secret (for key rotation)
bin/init-config.scm   write a new config file
src/app-config.sld    config reading (incl. (load ...)) and first-run creation
src/app-cli.sld       shared plumbing of the bin/ scripts
src/app-site.sld      URL prefix: url, asset, cookie-path
src/app-store.sld     atomic data files, ids, the data-dir lock
src/app-users.sld     users.scm and roles (user < admin)
src/app-auth.sld      sign-in, sessions (signed SameSite=Lax cookies), passwords
src/app-theme.sld     branding settings, theme.css, contrast check
src/app-views.sld     HTML pages (German UI)
src/app-web.sld       routes, handlers, middleware: make-app
static/               CSS and ES-module JavaScript (no build step)
tests/                run_all.sh; no server or ffmpeg needed
deploy/               systemd units, nginx/apache examples, README.Debian
package-deb.scm       builds the Debian package
```

The shared modules (config, site, store, users, auth, theme) are copies of
filme's. Fixes to them should go into both projects, until they move into
a common library.

## Tests

```sh
tests/run_all.sh
```

## Package

```sh
scm package-deb.scm           # dist/aufnahme_<version>_all.deb (needs a commit)
sudo apt install ./dist/aufnahme_*_all.deb
sudo -u aufnahme aufnahme-admin user add alice admin
```
