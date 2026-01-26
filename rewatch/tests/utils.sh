#!/bin/bash
overwrite() { echo -e "\r\033[1A\033[0K$@"; }
success() { echo -e "- ✅ \033[32m$1\033[0m"; }
error() { echo -e "- 🛑 \033[31m$1\033[0m"; }
bold() { echo -e "\033[1m$1\033[0m"; }
rewatch() {
  RUST_BACKTRACE=1 $REWATCH_EXECUTABLE $@
  local exit_code=$?
  # Wait for daemon to fully shut down after the client disconnects
  wait_for_daemon_stop
  return $exit_code
}
rewatch_bg() { RUST_BACKTRACE=1 $REWATCH_EXECUTABLE $@; }

# Get the daemon socket path from the socket path file
# Returns empty string if file doesn't exist
get_socket_path() {
  local socket_path_file="lib/bs/rescript.sock.path"
  if [ -f "$socket_path_file" ]; then
    cat "$socket_path_file"
  fi
}

# Kill the watcher and wait for daemon to shut down gracefully
# Requires WATCHER_PID to be set before calling
# Returns non-zero if daemon fails to shut down
exit_watcher() {
  # Send SIGINT (like Ctrl-C) to the watch client process.
  # This triggers the ctrlc handler which sends a Disconnect RPC
  # to the daemon before exiting. The daemon then shuts down when all
  # clients have disconnected.
  #
  # Note: WATCHER_PID may be a Node.js wrapper (cli/rescript.js) that spawns
  # the actual Rust binary. In background mode, the Node wrapper doesn't
  # forward SIGINT properly, so we find and signal the Rust process directly.
  if [ -n "$WATCHER_PID" ]; then
    # Find the actual rescript watch process (child of bash or Node wrapper)
    # The binary might be named "rescript" (direct) or "rescript.exe" (via Node wrapper)
    local rust_pid=$(pgrep -P $WATCHER_PID -f "rescript.* watch" 2>/dev/null)

    if [ -z "$rust_pid" ]; then
      # Maybe WATCHER_PID is already the Rust process
      rust_pid=$WATCHER_PID
    fi

    kill -INT $rust_pid 2>/dev/null || true

    # Wait for the process to exit with a timeout
    local timeout=10
    while kill -0 $rust_pid 2>/dev/null && [ $timeout -gt 0 ]; do
      sleep 0.5
      timeout=$((timeout - 1))
    done

    if kill -0 $rust_pid 2>/dev/null; then
      error "Watcher didn't exit after SIGINT, force killing"
      kill -9 $rust_pid 2>/dev/null || true
      kill -9 $WATCHER_PID 2>/dev/null || true
    fi
  fi

  # Wait for daemon to shut down gracefully (it should exit after disconnect)
  wait_for_daemon_stop 5

  # Verify daemon actually stopped
  local pid_file="lib/bs/rescript.pid"
  if [ -f "$pid_file" ]; then
    local daemon_pid=$(cat "$pid_file")
    if [ -n "$daemon_pid" ] && kill -0 $daemon_pid 2>/dev/null; then
      error "Daemon (PID $daemon_pid) failed to shut down after watch client disconnect"
      return 1
    fi
  fi
  return 0
}

# Check that the daemon has stopped (socket file removed)
# Usage: daemon_stopped || exit 1
daemon_stopped() {
  local socket_path=$(get_socket_path)
  if [ -n "$socket_path" ] && [ -e "$socket_path" ]; then
    error "Daemon socket still exists: $socket_path"
    return 1
  fi
  return 0
}

# Wait for the daemon to stop (socket file removed) with timeout
# Usage: wait_for_daemon_stop [timeout_seconds]
# Default timeout is 2 seconds
wait_for_daemon_stop() {
  local timeout=${1:-2}
  local count=0
  local interval=0.1
  local max_count=$(echo "$timeout / $interval" | bc)

  local socket_path=$(get_socket_path)

  while [ -n "$socket_path" ] && [ -e "$socket_path" ] && [ $count -lt $max_count ]; do
    sleep $interval
    count=$((count + 1))
  done
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
}

replace() {
  if [[ $OSTYPE == 'darwin'* ]];
  then
    sed -i '' $1 $2;
  else
    sed -i $1 $2;
  fi
}

# Wait for a file to exist (with timeout in seconds, default 30)
wait_for_file() {
  local file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    [ -f "$file" ] && return 0
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Wait for a pattern to appear in a file (with timeout in seconds, default 30)
wait_for_file_content() {
  local file="$1"; local pattern="$2"; local timeout="${3:-30}"
  while [ "$timeout" -gt 0 ]; do
    if [ -f "$file" ] && grep -q "$pattern" "$file" 2>/dev/null; then
      return 0
    fi
    sleep 1
    timeout=$((timeout - 1))
  done
  return 1
}

# Wait for the watcher to be ready (file watchers set up)
# by polling the log file for the "Watching for file changes" message.
# Usage: wait_for_watcher_ready <log_file> [timeout_seconds]
wait_for_watcher_ready() {
  local log_file="$1"; local timeout="${2:-30}"
  while [ "$timeout" -gt 0 ]; do
    if grep -q "Watching for file changes" "$log_file" 2>/dev/null; then
      return 0
    fi
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
