#!/bin/sh
set -eu

: "${REWATCH_OCAML_CHILD_STARTED:?}"
: "${REWATCH_OCAML_REAL_BSC:?}"

: > "$REWATCH_OCAML_CHILD_STARTED"
if [ -n "${REWATCH_OCAML_RELEASE_FILE:-}" ]; then
  while [ ! -f "$REWATCH_OCAML_RELEASE_FILE" ]; do
    sleep 0.05
  done
else
  sleep 5
fi
exec "$REWATCH_OCAML_REAL_BSC" "$@"
