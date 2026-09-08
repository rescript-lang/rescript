#!/bin/sh
set -eu

: "${REWATCH_REAL_BSC:?REWATCH_REAL_BSC must name the real compiler}"
: "${REWATCH_BSC_CALL_LOG:?REWATCH_BSC_CALL_LOG must name the call log}"

printf '%s\n' "$*" >> "$REWATCH_BSC_CALL_LOG"
exec "$REWATCH_REAL_BSC" "$@"
