#!/bin/sh
set -eu

: "${REWATCH_OCAML_COMPILE_STARTED:?}"
: "${REWATCH_OCAML_REAL_BSC:?}"
: "${REWATCH_OCAML_RELEASE_FILE:?}"

is_parser=false
for argument in "$@"; do
  if [ "$argument" = "-bs-ast" ]; then
    is_parser=true
    break
  fi
done

if [ "$is_parser" = false ]; then
  : >"$REWATCH_OCAML_COMPILE_STARTED"
  while [ ! -f "$REWATCH_OCAML_RELEASE_FILE" ]; do
    sleep 0.05
  done
fi

exec "$REWATCH_OCAML_REAL_BSC" "$@"
