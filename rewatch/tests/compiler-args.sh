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

# Additional check: warnings/error flags should be present in both parser_args and compiler_args (using namespace-casing package)
bold "Test: warnings/error flags appear in both parser_args and compiler_args (namespace-casing)"

stdout=$(rewatch compiler-args packages/namespace-casing/src/Consume.res 2>/dev/null)
if [ $? -ne 0 ]; then
  error "Error grabbing compiler args for packages/namespace-casing/src/Consume.res"
  exit 1
fi

# The package has warnings.number = +1000 and warnings.error = -2000
# Expect both parser_args and compiler_args to include -warn-error/-2000 and -w/+1000
warn_error_flag_count=$(echo "$stdout" | grep -F -o '"-warn-error"' | wc -l | xargs)
warn_error_val_count=$(echo "$stdout" | grep -F -o '"-2000"' | wc -l | xargs)
warn_number_flag_count=$(echo "$stdout" | grep -F -o '"-w"' | wc -l | xargs)
warn_number_val_count=$(echo "$stdout" | grep -F -o '"+1000"' | wc -l | xargs)

if [ "$warn_error_flag_count" -ne 2 ] || [ "$warn_error_val_count" -ne 2 ] || [ "$warn_number_flag_count" -ne 2 ] || [ "$warn_number_val_count" -ne 2 ]; then
  error "Expected -w/+1000 and -warn-error/-2000 to appear twice (parser_args and compiler_args)"
  echo "$stdout"
  exit 1
fi

success "warnings/error flags present in both parser and compiler args (namespace-casing)"
