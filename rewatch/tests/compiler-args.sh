#!/bin/bash
cd $(dirname $0)
source "./utils.sh"
cd ../testrepo

bold "Test: It should not matter where we grab the compiler-args for a file"
# Capture stdout for both invocations
stdout_root=$(rewatch compiler-args packages/file-casing/src/Consume.res 2>/dev/null)
stdout_pkg=$(cd packages/file-casing && "../../$REWATCH_EXECUTABLE" compiler-args src/Consume.res 2>/dev/null)

error_output=$(rewatch compiler-args packages/file-casing/src/Consume.res 2>&1)
if [ $? -ne 0 ]; then
  error "Error grabbing compiler args for packages/file-casing/src/Consume.res"
  printf "%s\n" "$error_output" >&2
  exit 1
fi
error_output=$(cd packages/file-casing && "../../$REWATCH_EXECUTABLE" compiler-args src/Consume.res 2>&1)
if [ $? -ne 0 ]; then
  error "Error grabbing compiler args for src/Consume.res in packages/file-casing"
  printf "%s\n" "$error_output" >&2
  exit 1
fi

# Compare the stdout of both runs; must be exactly identical
tmp1=$(mktemp); tmp2=$(mktemp)
trap 'rm -f "$tmp1" "$tmp2"' EXIT
printf "%s" "$stdout_root" > "$tmp1"
printf "%s" "$stdout_pkg" > "$tmp2"
if git diff --no-index --exit-code "$tmp1" "$tmp2" > /dev/null; then
  success "compiler-args stdout is identical regardless of cwd"
else
  error "compiler-args stdout differs depending on cwd"
  echo "---- diff ----"
  git diff --no-index "$tmp1" "$tmp2" || true
  exit 1
fi
