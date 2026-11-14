#!/bin/bash
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() { $REWATCH_EXECUTABLE $@; }
rewatch_bg() { nohup $REWATCH_EXECUTABLE $@; }

# Detect if running on Windows
is_windows() {
  [[ $OSTYPE == 'msys'* || $OSTYPE == 'cygwin'* || $OSTYPE == 'win'* ]];
}

process_is_running() {
  local pid="$1"
  if is_windows; then
    WATCH_PID="$pid" powershell.exe -NoProfile -NonInteractive -Command \
      'if (Get-Process -Id ([int]$env:WATCH_PID) -ErrorAction SilentlyContinue) { exit 0 } else { exit 1 }'
  else
    kill -0 "$pid" 2> /dev/null
  fi
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
}

replace() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' $1 $2;
  else
    sed -i $1 $2;
  fi
}

# Runs a command every 0.1 seconds until it succeeds or the timeout in
# seconds (the first argument) has passed. A one-second interval made every
# wait in the suite take up to a second longer than needed.
poll() {
  local deadline=$((SECONDS + $1)); shift
  until "$@"; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 0.1
  done
}

process_is_gone() { ! process_is_running "$1"; }

wait_for_pid_gone() {
  poll "${2:-20}" process_is_gone "$1"
}

exit_watcher() {
  local watcher_pid=""
  if [ -f lib/rescript/watch.lock ]; then
    watcher_pid=$(cat lib/rescript/watch.lock)
  fi

  # Windows refuses to delete a file while another process has it open
  # without delete sharing, so a watcher reading the lock can make one
  # attempt fail.
  local attempts=0
  until rm -f lib/rescript/watch.lock 2> /dev/null; do
    attempts=$((attempts + 1))
    if [ "$attempts" -ge 50 ]; then
      rm -f lib/rescript/watch.lock
      break
    fi
    sleep 0.1
  done

  if [ -n "$watcher_pid" ]; then
    if ! wait_for_pid_gone "$watcher_pid" 20; then
      error "Watcher process $watcher_pid did not exit after watch.lock was removed"
      kill_process "$watcher_pid"
      rm -f lib/rescript/watch.lock
      return 1
    fi
  fi
}

# Stop a watcher that ignored the shutdown protocol, so that it does not keep
# running into later tests.
kill_process() {
  local pid="$1"
  if is_windows; then
    WATCH_PID="$pid" powershell.exe -NoProfile -NonInteractive -Command \
      'Stop-Process -Id ([int]$env:WATCH_PID) -Force -ErrorAction SilentlyContinue'
  else
    kill -9 "$pid" 2> /dev/null
  fi
  wait_for_pid_gone "$pid" 5 || true
}

clear_locks() {
  rm -f lib/rescript/watch.lock lib/rescript/build.lock
}

file_exists() { [ -f "$1" ]; }
file_is_gone() { [ ! -f "$1" ]; }
file_has_pattern() { grep -q "$2" "$1" 2>/dev/null; }

file_lacks_pattern() {
  [ -f "$1" ] && ! grep -q "$2" "$1" 2>/dev/null
}

file_has_pattern_count() {
  local current_count
  current_count=$(grep -c "$2" "$1" 2>/dev/null || true)
  [ "${current_count:-0}" -ge "$3" ]
}

wait_for_file() { poll "${2:-60}" file_exists "$1"; }
wait_for_file_gone() { poll "${2:-60}" file_is_gone "$1"; }
wait_for_pattern() { poll "${3:-60}" file_has_pattern "$1" "$2"; }
wait_for_pattern_gone() { poll "${3:-60}" file_lacks_pattern "$1" "$2"; }

wait_for_pattern_count() {
  poll "${4:-60}" file_has_pattern_count "$1" "$2" "$3"
}
