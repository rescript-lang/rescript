#!/bin/sh
set -eu

: "${REWATCH_REAL_BSC:?REWATCH_REAL_BSC must name the real compiler}"
: "${REWATCH_BSC_CALL_LOG:?REWATCH_BSC_CALL_LOG must name the call log}"
: "${REWATCH_PARSE_WARNING_SOURCE:?REWATCH_PARSE_WARNING_SOURCE must name the source basename}"

printf '%s\n' "$*" >> "$REWATCH_BSC_CALL_LOG"

is_parse=false
matches_source=false
for argument in "$@"; do
  if [ "$argument" = -bs-ast ]; then
    is_parse=true
  fi
  if [ "$(basename "$argument")" = "$REWATCH_PARSE_WARNING_SOURCE" ]; then
    matches_source=true
  fi
done

status=0
"$REWATCH_REAL_BSC" "$@" || status=$?
if [ "$status" -eq 0 ] && [ "$is_parse" = true ] && \
  [ "$matches_source" = true ]; then
  printf '%s\n' REWATCH_PARSE_WARNING >&2
fi
exit "$status"
