#!/bin/sh
set -eu

is_parse=false
for argument in "$@"; do
  if [ "$argument" = "-bs-ast" ]; then
    is_parse=true
  fi
done

"$REWATCH_REAL_BSC" "$@"
status=$?
if [ "$status" -eq 0 ] && [ "$is_parse" = false ] && \
  [ ! -e "$REWATCH_SOURCE_DELETED" ]; then
  rm "$REWATCH_SOURCE_TO_DELETE"
  : > "$REWATCH_SOURCE_DELETED"
fi
exit "$status"
