#!/bin/bash
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() { RUST_BACKTRACE=1 $REWATCH_EXECUTABLE $@; }
rewatch_bg() { RUST_BACKTRACE=1 nohup $REWATCH_EXECUTABLE $@; }

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

wait_for_pid_gone() {
  local pid="$1"; local timeout
  timeout=${2:-20}
  while process_is_running "$pid" && [ "$timeout" -gt 0 ]; do
    sleep 1
    timeout=$((timeout - 1))
  done
  ! process_is_running "$pid"
}

exit_watcher() {
  local watcher_pid=""
  if [ -f lib/watch.lock ]; then
    watcher_pid=$(cat lib/watch.lock)
  fi

  # Windows refuses to delete a file while another process has it open
  # without delete sharing, so a watcher reading the lock can make one
  # attempt fail.
  local attempts=0
  until rm -f lib/watch.lock 2> /dev/null; do
    attempts=$((attempts + 1))
    if [ "$attempts" -ge 50 ]; then
      rm -f lib/watch.lock
      break
    fi
    sleep 0.1
  done

  if [ -n "$watcher_pid" ]; then
    if ! wait_for_pid_gone "$watcher_pid" 20; then
      error "Watcher process $watcher_pid did not exit after watch.lock was removed"
      kill_process "$watcher_pid"
      rm -f lib/watch.lock
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
  rm -f lib/watch.lock lib/build.lock
}

wait_for_file() {
  local file="$1"; local timeout
  timeout=${2:-60}
  while [ "$timeout" -gt 0 ]; do
    [ -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_pattern() {
  local file="$1"; local pattern="$2"; local timeout
  timeout=${3:-60}
  while [ "$timeout" -gt 0 ]; do
    grep -q "$pattern" "$file" 2>/dev/null && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_pattern_count() {
  local file="$1"; local pattern="$2"; local expected="$3"; local timeout
  timeout=${4:-60}
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

wait_for_pattern_gone() {
  local file="$1"; local pattern="$2"; local timeout
  timeout=${3:-60}
  while [ "$timeout" -gt 0 ]; do
    if [ -f "$file" ] && ! grep -q "$pattern" "$file" 2>/dev/null; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

wait_for_file_gone() {
  local file="$1"; local timeout
  timeout=${2:-60}
  while [ "$timeout" -gt 0 ]; do
    [ ! -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}
