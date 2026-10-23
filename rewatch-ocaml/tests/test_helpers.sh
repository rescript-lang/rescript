# Helpers shared by the OCaml rewatch test scripts. Source this file; it is
# POSIX sh because run.sh runs under sh. Without `local`, the helpers keep their
# state in `_`-prefixed variables so they never overwrite a caller's.
#
# The wait helpers poll every 0.1 s, 200 times (20 s) unless a count of
# attempts is given, and describe what they waited for on stderr when they
# time out.

windows_posix_shell=false
case $(uname -s) in
  CYGWIN*|MINGW*|MSYS*) windows_posix_shell=true ;;
esac

# Sets _wait_line to " (line N)" for the line that called the wait helper,
# under bash. dash never parses the eval'ed text.
_wait_caller_line() {
  _wait_line=
  if [ -n "${BASH_VERSION:-}" ]; then
    eval '_wait_line=" (line ${BASH_LINENO[1]:-?})"'
  fi
}

# wait_for_file PATH [ATTEMPTS]
wait_for_file() {
  _wait_path=$1
  _wait_attempts=${2:-200}
  while [ "$_wait_attempts" -gt 0 ]; do
    if [ -f "$_wait_path" ]; then return 0; fi
    _wait_attempts=$((_wait_attempts - 1))
    sleep 0.1
  done
  _wait_caller_line
  printf 'Timed out waiting for file %s%s\n' "$_wait_path" "$_wait_line" >&2
  return 1
}

# wait_for_text PATH TEXT [COUNT] [ATTEMPTS]: waits until PATH contains TEXT
# on at least COUNT lines (default 1).
wait_for_text() {
  _wait_path=$1
  _wait_text=$2
  _wait_count=${3:-1}
  _wait_attempts=${4:-200}
  while [ "$_wait_attempts" -gt 0 ]; do
    _wait_found=$(grep -cF -- "$_wait_text" "$_wait_path" 2>/dev/null || true)
    if [ "${_wait_found:-0}" -ge "$_wait_count" ]; then return 0; fi
    _wait_attempts=$((_wait_attempts - 1))
    sleep 0.1
  done
  _wait_caller_line
  printf 'Timed out waiting for %s line(s) with %s in %s%s\n' \
    "$_wait_count" "$_wait_text" "$_wait_path" "$_wait_line" >&2
  if [ -f "$_wait_path" ]; then
    printf '%s\n' '--- observed contents ---' >&2
    cat "$_wait_path" >&2
  else
    printf '%s\n' '--- file does not exist ---' >&2
  fi
  return 1
}

# wait_for_watch LOG SUFFIX [ATTEMPTS]: waits until a -v watcher logs a watch
# handle for a path ending in SUFFIX, an extended regular expression.
wait_for_watch() {
  _wait_path=$1
  _wait_suffix=$2
  _wait_attempts=${3:-200}
  while [ "$_wait_attempts" -gt 0 ]; do
    if grep -Eq "watching \((non-)?recursive\): .*$_wait_suffix[[:space:]]*\$" \
        "$_wait_path" 2>/dev/null; then
      return 0
    fi
    _wait_attempts=$((_wait_attempts - 1))
    sleep 0.1
  done
  _wait_caller_line
  printf 'Timed out waiting for a watch on %s in %s%s\n' \
    "$_wait_suffix" "$_wait_path" "$_wait_line" >&2
  if [ -f "$_wait_path" ]; then cat "$_wait_path" >&2; fi
  return 1
}

# wait_for_pid_gone PID [ATTEMPTS]
wait_for_pid_gone() {
  _wait_pid=$1
  _wait_attempts=${2:-200}
  while [ "$_wait_attempts" -gt 0 ]; do
    if ! kill -0 "$_wait_pid" 2>/dev/null; then return 0; fi
    _wait_attempts=$((_wait_attempts - 1))
    sleep 0.1
  done
  _wait_caller_line
  printf 'Timed out waiting for process %s to exit%s\n' \
    "$_wait_pid" "$_wait_line" >&2
  return 1
}

# file_link_if_supported TARGET LINK: fails where the platform or its
# permissions do not allow file symlinks, as on Windows without developer mode.
file_link_if_supported() {
  _link_target=$1
  _link_path=$2
  if $windows_posix_shell; then
    LINK_PATH=$(cygpath -aw "$_link_path") TARGET_PATH=$(cygpath -aw "$_link_target") \
      powershell.exe -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; $null = New-Item -ItemType SymbolicLink -Path $env:LINK_PATH -Target $env:TARGET_PATH' \
        >/dev/null 2>&1
  else
    ln -s "$_link_target" "$_link_path"
  fi
}

# directory_link TARGET LINK: a symlink, or a junction on Windows, which needs
# no special permissions.
directory_link() {
  _link_target=$1
  _link_path=$2
  if $windows_posix_shell; then
    LINK_PATH=$(cygpath -aw "$_link_path") TARGET_PATH=$(cygpath -aw "$_link_target") \
      powershell.exe -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; $null = New-Item -ItemType Junction -Path $env:LINK_PATH -Target $env:TARGET_PATH'
  else
    ln -s "$_link_target" "$_link_path"
  fi
}
