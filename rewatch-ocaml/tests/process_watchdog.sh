# Source this file and call start_process_watchdog to run the rest of the
# shell under windows_process_watchdog.ps1. It does nothing on other systems.
# The shell exits with status 1 when the watchdog had to stop processes.

start_process_watchdog() {
  case "${RUNNER_OS:-}" in
  Windows) ;;
  *) return 0 ;;
  esac
  watchdog_dir=$(mktemp -d)
  export WATCHDOG_RUNNING="$watchdog_dir/running"
  export WATCHDOG_REPORT="$watchdog_dir/report"
  : >"$WATCHDOG_RUNNING"
  powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass \
    -File "$(dirname "${BASH_SOURCE[0]}")/windows_process_watchdog.ps1" &
  trap stop_process_watchdog EXIT
}

stop_process_watchdog() {
  local status=$?
  rm -f "$WATCHDOG_RUNNING"
  if [ -s "$WATCHDOG_REPORT" ]; then
    echo "::error::The process watchdog stopped runaway test processes:"
    cat "$WATCHDOG_REPORT"
    status=1
  fi
  rm -rf "$watchdog_dir"
  exit "$status"
}
