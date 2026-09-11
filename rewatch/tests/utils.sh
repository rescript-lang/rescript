#!/bin/bash
overwrite() { echo -e "\r\033[1A\033[0K$@"; }
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() { RUST_BACKTRACE=1 $REWATCH_EXECUTABLE $@; }
rewatch_bg() { RUST_BACKTRACE=1 nohup $REWATCH_EXECUTABLE $@; }

restore_tracked_files() {
  local repo_root path target temporary
  if ! repo_root=$(git rev-parse --show-toplevel); then
    error "Could not locate repository while restoring: $*"
    exit 1
  fi
  while IFS= read -r -d '' path; do
    target="$repo_root/$path"
    temporary="$target.rewatch-restore-$$"
    if ! git show ":$path" > "$temporary" || ! mv "$temporary" "$target"; then
      rm -f "$temporary"
      error "Could not restore tracked test fixture: $path"
      exit 1
    fi
  done < <(git ls-files --full-name --modified --deleted -z -- "$@")
}

# Detect if running on Windows
is_windows() {
  [[ $OSTYPE == 'msys'* || $OSTYPE == 'cygwin'* || $OSTYPE == 'win'* ]];
}

# get pwd with forward slashes
pwd_prefix() {
  if is_windows; then
    # On Windows, escape backslashes for sed and convert to forward slashes for consistent snapshots
    # This ensures paths like C:\a\b are replaced correctly
    # First get the Windows-style path with backslashes
    local win_path=$(pwd -W | sed "s#/#\\\\#g")
    # Then escape the backslashes for sed replacement
    echo $win_path | sed 's#\\#\\\\#g'
  else
    # On Unix-like systems, escape forward slashes for sed
    echo $(pwd | sed "s#/#\\/#g")
  fi
}

# replace the absolute path so the snapshot is the same on all machines
# then normalize the path separators
normalize_paths() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' "s#$(pwd_prefix)##g" $1;
  else
    if is_windows; then
      sed -i "s#$(pwd_prefix)##g" $1
      sed -i "s#\\\\#/#g" $1
    else
      sed -i "s#$(pwd_prefix)##g" $1;
    fi
  fi

  # Compiler diagnostics can contain one additional trailing blank line on
  # Windows. Keep snapshot comparisons focused on the stable two-line
  # separation before package configuration diagnostics.
  local normalized="$1.rewatch-normalize-$$"
  awk '
    {
      sub(/\r$/, "")
      if ($0 == "") {
        blank_count++
        next
      }
      blanks = blank_count
      if ($0 ~ /Package .* uses deprecated config/ && blanks > 2) {
        blanks = 2
      }
      for (i = 0; i < blanks; i++) print ""
      blank_count = 0
      print
    }
    END {
      for (i = 0; i < blank_count; i++) print ""
    }
  ' "$1" > "$normalized"
  mv "$normalized" "$1"
}

replace() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' $1 $2;
  else
    sed -i $1 $2;
  fi
}

normalize_belt_portal_import() {
  local output="./packages/dep02/src/Array.mjs"
  if [ -f "$output" ]; then
    replace 's#@rescript/belt/src/#@rescript/belt/lib/es6/src/#g' "$output"
  fi
}

wait_for_pid_gone() {
  local pid="$1"; local timeout="${2:-10}"
  while kill -0 "$pid" 2> /dev/null && [ "$timeout" -gt 0 ]; do
    sleep 1
    timeout=$((timeout - 1))
  done
  ! kill -0 "$pid" 2> /dev/null
}

exit_watcher() {
  local watcher_pid=""
  if [ -f lib/watch.lock ]; then
    watcher_pid=$(cat lib/watch.lock)
  fi

  rm -f lib/watch.lock

  if [ -n "$watcher_pid" ]; then
    if ! wait_for_pid_gone "$watcher_pid" 10; then
      error "Watcher process $watcher_pid did not exit after watch.lock was removed"
      return 1
    fi
  fi
}

clear_locks() {
  rm -f lib/watch.lock lib/build.lock
}

wait_for_file() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_pattern_count() {
  local file="$1"; local pattern="$2"; local expected="$3"; local timeout="${4:-30}"
  while [ "$timeout" -gt 0 ]; do
    local current_count
    current_count=$(grep -c "$pattern" "$file" 2>/dev/null || true)
    current_count=${current_count:-0}
    [ "$current_count" -ge "$expected" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_file_gone() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ ! -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}
