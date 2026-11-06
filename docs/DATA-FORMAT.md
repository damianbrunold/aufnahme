# aufnahme data format (version 1)

All state lives in one folder, `data-dir` (`/var/lib/aufnahme` when
installed from the package, `var/` in a development checkout). Every
metadata file is a single Scheme datum written with `write`. You can read
it with any Scheme `read`, or with a text editor. Files are replaced
atomically: the new content is written to a temporary name and then renamed
over the old file.

```
data-dir/
  users.scm                     users (as in filme), plus each user's filme token
  site/settings.scm             branding: site name, colours, logo, favicon
  projects/<pid>/
    project.scm                 the project (below)
    export.mp4                  the last export (H.264/AAC, faststart)
    takes/<tid>/
      take.scm                  the take (below)
      screen.part, cam.part     raw recorder output while recording
      screen.webm|mp4           the screen track after processing (remuxed, lossless)
      cam.webm|mp4              the webcam track after processing, if any
      thumb.jpg                 poster frame
      strip.jpg                 film strip for the editor (160x90 per frame)
      wave.png                  waveform for the editor (only with audio)
```

Ids are 11 random URL-safe characters (`A–Z a–z 0–9 - _`).

## `projects/<pid>/project.scm`

| key | value |
|---|---|
| `version` | `1` |
| `id`, `title`, `owner` | strings (`owner` is a user name) |
| `created`, `updated` | Unix seconds |
| `takes` | take ids in recording order |
| `segments` | the cut list: `((take . tid) (start . s) (end . s))` in playing order; missing means every take whole |
| `webcam` | `((on . #t) (corner . bottom-right) (size . 0.22) (margin . 24))`; `corner` is `top-left`, `top-right`, `bottom-left` or `bottom-right`, and `size` is a fraction of the output width |
| `loudnorm` | `#t` / `#f`: EBU R128 loudness normalization on export |
| `export` | `((status . none\|queued\|running\|ready\|failed) (progress . 0..1) (error . #f) (finished . secs) (duration . s) (size . bytes))` |
| `publish` | `((status . none\|queued\|running\|done\|failed) (progress . 0..1) (error . #f) (video . filme-id) (url . edit-url) (published . secs) (export . finished-of-the-published-export) (title . s) (description . s) (tags . s))` |

## `takes/<tid>/take.scm`

| key | value |
|---|---|
| `version`, `id`, `created` | as above |
| `status` | `recording`, `processing`, `ready` or `failed` |
| `error` | `#f`, or the processing error |
| `screen`, `cam` | `((mime . "video/webm;codecs=vp9,opus") (start . ms) (chunks . n) (bytes . n) (file . "screen.webm"))`; `cam` is `#f` without a webcam. `start` is the time (ms since the epoch) the browser's recorder started; `chunks` and `bytes` count what has arrived so far |
| `duration`, `width`, `height` | of the screen track |
| `has-audio` | whether the screen track has sound |
| `cam-offset` | seconds the webcam recording started after the screen recording (negative: earlier) |
| `strip` | number of frames in `strip.jpg` (absent until made) |

## `users.scm`

As in filme, with roles `user` and `admin`. A user may also have
`(filme-token . "fltk_...")`: the personal API token used to publish to
filme. It is stored in plain text, because aufnahme has to send it.

## Backup and restore

Copy the folder, together with `/etc/aufnahme/config.scm`, whose
`session-secret` keeps sessions valid. To restore, stop the service, copy
the folder back, make it owned by `aufnahme:aufnahme`, and start the
service.

Exports can always be rendered again from the recordings. A backup can
leave out `export.mp4`, but then each export has to be redone.
