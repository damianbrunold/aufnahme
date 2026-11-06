#!/bin/sh
# Runs every test file. test_media.scm needs ffmpeg/ffprobe on the PATH.
cd "$(dirname "$0")/.." || exit 1
status=0
for t in tests/test_*.scm; do
  scm "$t" || status=1
done
exit $status
