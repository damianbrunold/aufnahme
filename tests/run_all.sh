#!/bin/sh
# Runs every test file.
cd "$(dirname "$0")/.." || exit 1
status=0
for t in tests/test_*.scm; do
  scm "$t" || status=1
done
exit $status
