#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
if [ "$#" -ne 2 ]; then
  echo "usage: $0 WINDOWS_CC WINDOWS_OCAML_INCLUDE" >&2
  exit 2
fi

compiler=$1
ocaml_include=$2
object=$(mktemp "${TMPDIR:-/tmp}/rewatch-windows-job-stub-XXXXXX.o")
trap 'rm -f "$object"' EXIT

if ! printf '#ifndef _WIN32\n#error compiler does not target Windows\n#endif\n' \
  | "$compiler" -x c -E - >/dev/null 2>&1; then
  echo "$compiler does not target Windows" >&2
  exit 2
fi

"$compiler" -Wall -Wextra -Werror -I "$ocaml_include" -c \
  "$root/rewatch-ocaml/windows_job_stubs.c" -o "$object"
