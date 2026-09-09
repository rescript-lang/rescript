#!/bin/sh
set -eu

is_parse=false
output=""
previous=""
for argument in "$@"; do
  if [ "$previous" = "-o" ]; then
    output=$argument
  fi
  if [ "$argument" = "-bs-ast" ]; then
    is_parse=true
  fi
  previous=$argument
done

"$REWATCH_REAL_BSC" "$@"
status=$?
if [ "$status" -eq 0 ] && [ "$is_parse" = true ] && [ -n "$output" ] && \
  [ ! -e "$REWATCH_AST_DELETED" ]; then
  rm "$output"
  : > "$REWATCH_AST_DELETED"
fi
exit "$status"
