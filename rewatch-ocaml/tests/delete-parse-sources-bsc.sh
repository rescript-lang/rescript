#!/bin/sh
set -eu

is_parse=false
for argument in "$@"; do
  if [ "$argument" = "-bs-ast" ]; then
    is_parse=true
  fi
done

if [ "$is_parse" = true ] && [ ! -e "$REWATCH_SOURCES_DELETED" ]; then
  rm -f "$REWATCH_SOURCE_A" "$REWATCH_SOURCE_B"
  : > "$REWATCH_SOURCES_DELETED"
fi

exec "$REWATCH_REAL_BSC" "$@"
