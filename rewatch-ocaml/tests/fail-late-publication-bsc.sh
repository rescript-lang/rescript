#!/bin/sh
set -eu

is_parse=false
compiles_target=false
for argument in "$@"; do
  if [ "$argument" = "-bs-ast" ]; then
    is_parse=true
  fi
  case "$argument" in
    */A.ast | A.ast) compiles_target=true ;;
  esac
done

"$REWATCH_REAL_BSC" "$@"
status=$?
if [ "$status" -eq 0 ] && [ "$is_parse" = false ] && \
  [ "$compiles_target" = true ] && \
  [ -e "$REWATCH_FAIL_PUBLICATION" ] && \
  [ ! -e "$REWATCH_PUBLICATION_FAILED" ]; then
  rm -f "$REWATCH_PUBLICATION_DESTINATION"
  mkdir "$REWATCH_PUBLICATION_DESTINATION"
  : > "$REWATCH_PUBLICATION_FAILED"
fi
exit "$status"
