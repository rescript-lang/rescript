#!/usr/bin/env bash
#
# Reactive Server Test Harness
#
# Tests the reanalyze-server's incremental behavior by:
# 1. Starting the server
# 2. Running an initial build and capturing baseline
# 3. Making a predictable source change (add unused value)
# 4. Rebuilding and verifying the delta (+1 issue)
# 5. Reverting and verifying the delta goes back to 0
# 6. Stopping the server
#
# Can be run multiple times with --iterations N to check for non-determinism.
# Use --project to run on a different project (e.g., the benchmark).

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
TOOLS_BIN="$REPO_ROOT/_build/default/tools/bin/main.exe"
SERVER_PID=""
ITERATIONS=3
VERBOSE=0
TIMING=1

# Project-specific settings (can be overridden with --project)
PROJECT_DIR="$SCRIPT_DIR"
TEST_FILE=""  # Will be set based on project
REANALYZE_ARGS="-json"
EXISTING_DEAD_VALUE=""  # A value that exists and is dead, to make live in scenario B

# Timing helpers - use bash built-in EPOCHREALTIME for low overhead
declare -A TIMING_TOTALS
time_start() {
  if [[ $TIMING -eq 1 ]]; then
    TIMING_START="${EPOCHREALTIME:-$(date +%s.%N)}"
  fi
}
time_end() {
  if [[ $TIMING -eq 1 ]]; then
    local label="$1"
    local end="${EPOCHREALTIME:-$(date +%s.%N)}"
    # Use bc for floating point arithmetic (much faster than python)
    local elapsed=$(echo "$end - $TIMING_START" | bc)
    TIMING_TOTALS[$label]=$(echo "${TIMING_TOTALS[$label]:-0} + $elapsed" | bc)
    log_verbose "  ⏱ $label: ${elapsed}s"
  fi
}

BACKUP_FILE="/tmp/reactive-test-backup.$$"
DEFAULT_SOCKET_FILE=""

# Cleanup function
cleanup() {
  # Restore test file if backup exists
  if [[ -f "$BACKUP_FILE" ]]; then
    cp "$BACKUP_FILE" "$TEST_FILE"
    rm -f "$BACKUP_FILE"
  fi
  # Stop server if running
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  # Best-effort cleanup of default socket (server should also clean this up)
  if [[ -n "${DEFAULT_SOCKET_FILE:-}" ]]; then
    rm -f "$DEFAULT_SOCKET_FILE" 2>/dev/null || true
  fi
}
trap cleanup EXIT

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  --iterations N   Run the test N times (default: 3)
  --project PATH   Use a different project directory
  --verbose        Print detailed output
  --help           Show this help

Projects:
  deadcode         Standard test project (default)
  benchmark        Large benchmark project (deadcode-benchmark)

Examples:
  $0 --iterations 5 --verbose
  $0 --project ../deadcode-benchmark
EOF
}

# Logging with phase-specific prefixes and colors
log() {
  echo -e "${GREEN}[TEST]${NC} $*"
}

log_build() {
  echo -e "${YELLOW}[BUILD]${NC} $*"
}

log_server() {
  echo -e "${MAGENTA}[SERVER]${NC} $*"
}

log_reactive() {
  echo -e "${CYAN}[REACTIVE]${NC} $*"
}

log_standalone() {
  echo -e "${BLUE}[STANDALONE]${NC} $*"
}

log_edit() {
  echo -e "${BOLD}[EDIT]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*"
}

log_verbose() {
  if [[ $VERBOSE -eq 1 ]]; then
    echo -e "${YELLOW}[DEBUG]${NC} $*"
  fi
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --iterations)
      ITERATIONS="$2"
      shift 2
      ;;
    --project)
      PROJECT_DIR="$(cd "$2" && pwd)"
      shift 2
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      log_error "Unknown option: $1"
      usage
      exit 1
      ;;
  esac
done

# Configure project-specific settings
configure_project() {
  local project_name
  project_name="$(basename "$PROJECT_DIR")"
  
  case "$project_name" in
    deadcode)
      TEST_FILE="$PROJECT_DIR/src/DeadValueTest.res"
      EXISTING_DEAD_VALUE="subList"
      ;;
    deadcode-benchmark)
      TEST_FILE="$PROJECT_DIR/src/AutoAnnotate_1.res"
      EXISTING_DEAD_VALUE=""  # Will skip scenario B
      ;;
    *)
      # Generic fallback - try to find a .res file
      TEST_FILE="$(find "$PROJECT_DIR/src" -name "*.res" -type f | head -1)"
      EXISTING_DEAD_VALUE=""
      ;;
  esac
  
  if [[ -z "$TEST_FILE" || ! -f "$TEST_FILE" ]]; then
    log_error "Could not find test file for project: $project_name"
    exit 1
  fi
}

configure_project

# Ensure we're in the project directory
cd "$PROJECT_DIR"

# Check that the tools binary exists
if [[ ! -x "$TOOLS_BIN" ]]; then
  log_error "Tools binary not found: $TOOLS_BIN"
  log_error "Run 'make' from the repo root to build it."
  exit 1
fi

# Clean and build the project (ensure reproducible state)
log_build "Clean..."
yarn clean > /dev/null 2>&1
log_build "Initial build..."
time_start
yarn build > /dev/null 2>&1
time_end "initial_build"

# Backup the test file
cp "$TEST_FILE" "$BACKUP_FILE"

# Start the server
start_server() {
  DEFAULT_SOCKET_FILE="$PROJECT_DIR/.rescript-reanalyze.sock"
  log_server "Starting (socket: $DEFAULT_SOCKET_FILE)..."
  rm -f "$DEFAULT_SOCKET_FILE" 2>/dev/null || true

  time_start
  # shellcheck disable=SC2086
  "$TOOLS_BIN" reanalyze-server \
    > /tmp/reanalyze-server-$$.log 2>&1 &
  SERVER_PID=$!
  
  # Wait for socket to appear
  for i in {1..30}; do
    if [[ -S "$DEFAULT_SOCKET_FILE" ]]; then
      time_end "server_startup"
      log_verbose "Server ready (socket exists)"
      return 0
    fi
    sleep 0.1
  done
  
  log_error "Server failed to start. Log:"
  cat /tmp/reanalyze-server-$$.log
  return 1
}

stop_server() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    log_server "Stopping (pid $SERVER_PID)..."
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  if [[ -n "${DEFAULT_SOCKET_FILE:-}" ]]; then
    rm -f "$DEFAULT_SOCKET_FILE" 2>/dev/null || true
  fi
  SERVER_PID=""
}

# Send a reactive request (via server) and capture JSON output
# Use label to distinguish warm vs incremental
send_request() {
  local output_file="$1"
  local label="${2:-reactive}"
  time_start
  # shellcheck disable=SC2086
  "$TOOLS_BIN" reanalyze $REANALYZE_ARGS > "$output_file" 2>/dev/null
  time_end "$label"
}

# Run standalone (non-reactive) analysis for comparison
run_standalone_analysis() {
  local output_file="$1"
  time_start
  # shellcheck disable=SC2086
  RESCRIPT_REANALYZE_NO_SERVER=1 "$TOOLS_BIN" reanalyze $REANALYZE_ARGS > "$output_file" 2>/dev/null
  time_end "standalone"
}

# Count issues in JSON output
count_issues() {
  local json_file="$1"
  python3 -c "import json; print(len(json.load(open('$json_file'))))"
}

# Compare two JSON files and return added/removed counts
compare_json() {
  local before="$1"
  local after="$2"
  time_start
  python3 - <<PY
import json
import os

def key(i):
    return (i.get('name'), i.get('file'), tuple(i.get('range',[])), i.get('message'))

def short_file(path):
    # Show just filename, not full path
    return os.path.basename(path) if path else '?'

def format_range(r):
    # range is [startLine, startCol, endLine, endCol] (0-indexed)
    if len(r) >= 2:
        return f"L{r[0]+1}:{r[1]}"
    return "?"

before = set(map(key, json.load(open('$before'))))
after = set(map(key, json.load(open('$after'))))
added = after - before
removed = before - after
print(f"added={len(added)} removed={len(removed)}")

# Check for location-only changes (same name+message, different range)
added_by_msg = {(a[0], a[3]): a for a in added}
removed_by_msg = {(r[0], r[3]): r for r in removed}
location_shifts = set(added_by_msg.keys()) & set(removed_by_msg.keys())

# Print location shifts first (less alarming)
for msg_key in sorted(location_shifts):
    a = added_by_msg[msg_key]
    r = removed_by_msg[msg_key]
    print(f"  ~ {a[0]}: {a[3][:50]}... (moved {format_range(r[2])} -> {format_range(a[2])})")

# Print true additions (not just location shifts)
for a in sorted(added):
    if (a[0], a[3]) not in location_shifts:
        print(f"  + {a[0]} @ {short_file(a[1])}:{format_range(a[2])}: {a[3][:50]}...")

# Print true removals (not just location shifts)
for r in sorted(removed):
    if (r[0], r[3]) not in location_shifts:
        print(f"  - {r[0]} @ {short_file(r[1])}:{format_range(r[2])}: {r[3][:50]}...")
PY
  time_end "json_compare"
}

# Add an unused value to the test file (creates +1 dead code warning)
add_unused_value() {
  log_verbose "Adding unused value to $TEST_FILE"
  # Insert after line 4 (after valueOnlyInImplementation)
  sed -i.bak '5a\
/* Reactive test: this unused value should be detected */\
let reactiveTestUnusedValue = 999\
' "$TEST_FILE"
  rm -f "$TEST_FILE.bak"
}

# Add a usage for the existing dead value (removes its dead code warning)
add_usage_for_dead_value() {
  log_verbose "Adding usage for $EXISTING_DEAD_VALUE in $TEST_FILE"
  # Append usage at end of file
  echo "
/* Reactive test: make $EXISTING_DEAD_VALUE live */
let _ = ${EXISTING_DEAD_VALUE}(0, 1, list{1, 2, 3})
" >> "$TEST_FILE"
}

# Restore the original test file
restore_test_file() {
  log_verbose "Restoring original $TEST_FILE"
  cp "$BACKUP_FILE" "$TEST_FILE"
}

# Rebuild the project
rebuild_project() {
  log_build "Rebuilding..."
  time_start
  yarn build > /dev/null 2>&1
  time_end "rebuild"
}

# Run Scenario A: Add dead code (+1 issue)
run_scenario_add_dead() {
  local baseline_file="$1"
  local baseline_count="$2"
  local after_file="/tmp/reanalyze-after-add-$$.json"
  local cold_file="/tmp/reanalyze-cold-$$.json"
  
  log_edit "Adding unused value to $(basename "$TEST_FILE") (creates dead code)..."
  add_unused_value
  rebuild_project
  
  # Run incremental reactive (after file change) and cold analysis
  log_reactive "Analyzing (incremental)..."
  send_request "$after_file" "incremental"
  log_standalone "Analyzing..."
  run_standalone_analysis "$cold_file"
  
  local after_count
  after_count=$(count_issues "$after_file")
  log_verbose "After add: $after_count issues"
  
  # Compare
  log "Delta:"
  compare_json "$baseline_file" "$after_file"
  
  # Check that reactiveTestUnusedValue is in the issues
  if ! grep -q "reactiveTestUnusedValue" "$after_file"; then
    log_error "Expected to find 'reactiveTestUnusedValue' in issues"
    rm -f "$after_file" "$cold_file"
    return 1
  fi
  log "✓ Found reactiveTestUnusedValue in new issues"
  
  log_edit "Reverting $(basename "$TEST_FILE")..."
  restore_test_file
  rebuild_project
  
  log_reactive "Analyzing (incremental)..."
  send_request "$after_file" "incremental"
  log_standalone "Analyzing..."
  run_standalone_analysis "$cold_file"
  after_count=$(count_issues "$after_file")
  
  if [[ "$after_count" -ne "$baseline_count" ]]; then
    log_error "Expected $baseline_count issues after revert, got $after_count"
    rm -f "$after_file" "$cold_file"
    return 1
  fi
  log "✓ Back to baseline"
  
  rm -f "$after_file" "$cold_file"
  return 0
}

# Run Scenario B: Make dead code live (-1 issue)
run_scenario_make_live() {
  local baseline_file="$1"
  local baseline_count="$2"
  local after_file="/tmp/reanalyze-after-live-$$.json"
  local cold_file="/tmp/reanalyze-cold-$$.json"
  
  # Skip if no existing dead value configured for this project
  if [[ -z "$EXISTING_DEAD_VALUE" ]]; then
    log "Skipping scenario B (no existing dead value configured)"
    return 0
  fi
  
  # Check that the warning exists in baseline
  if ! grep -q "$EXISTING_DEAD_VALUE is never used" "$baseline_file"; then
    log_warn "$EXISTING_DEAD_VALUE warning not in baseline - skipping scenario B"
    return 0
  fi
  
  log_edit "Adding usage for $EXISTING_DEAD_VALUE (makes it live)..."
  add_usage_for_dead_value
  rebuild_project
  
  # Run incremental reactive (after file change) and cold analysis
  log_reactive "Analyzing (incremental)..."
  send_request "$after_file" "incremental"
  log_standalone "Analyzing..."
  run_standalone_analysis "$cold_file"
  
  local after_count
  after_count=$(count_issues "$after_file")
  log_verbose "After making live: $after_count issues"
  
  # Compare
  log "Delta:"
  compare_json "$baseline_file" "$after_file"
  
  # Check that the warning is gone
  if grep -q "$EXISTING_DEAD_VALUE is never used" "$after_file"; then
    log_error "$EXISTING_DEAD_VALUE should no longer be dead"
    rm -f "$after_file" "$cold_file"
    return 1
  fi
  log "✓ $EXISTING_DEAD_VALUE warning removed"
  
  log_edit "Reverting $(basename "$TEST_FILE")..."
  restore_test_file
  rebuild_project
  
  log_reactive "Analyzing (incremental)..."
  send_request "$after_file" "incremental"
  log_standalone "Analyzing..."
  run_standalone_analysis "$cold_file"
  after_count=$(count_issues "$after_file")
  
  if [[ "$after_count" -ne "$baseline_count" ]]; then
    log_error "Expected $baseline_count issues after revert, got $after_count"
    rm -f "$after_file" "$cold_file"
    return 1
  fi
  log "✓ Back to baseline"
  
  rm -f "$after_file" "$cold_file"
  return 0
}

# Run one benchmark iteration
run_iteration() {
  local iter="$1"
  local baseline_file="$2"
  local baseline_count="$3"
  
  log "=== Iteration $iter/$ITERATIONS ==="
  
  # Run Scenario A: Add dead code
  if ! run_scenario_add_dead "$baseline_file" "$baseline_count"; then
    return 1
  fi
  
  # Run Scenario B: Make dead code live
  if ! run_scenario_make_live "$baseline_file" "$baseline_count"; then
    return 1
  fi
  
  log "✓ Iteration $iter passed"
  return 0
}

# Main
main() {
  log "Reactive Server Test Harness"
  log "Project: $(basename "$PROJECT_DIR") ($PROJECT_DIR)"
  log "Test file: $(basename "$TEST_FILE")"
  log "Iterations: $ITERATIONS"
  log ""
  
  start_server
  
  #-----------------------------------------
  # WARMUP PHASE: Populate cache
  #-----------------------------------------
  log "=== WARMUP ==="
  local baseline_file="/tmp/reanalyze-baseline-$$.json"
  local cold_file="/tmp/reanalyze-cold-$$.json"
  
  # First request populates the cache
  log_reactive "Analyzing (first request, populates cache)..."
  send_request "$baseline_file" "cold_reactive"
  
  # Second request uses warm cache
  log_reactive "Analyzing (warm, verifies cache)..."
  send_request "$cold_file" "warm"
  rm -f "$cold_file"
  
  local baseline_count
  baseline_count=$(count_issues "$baseline_file")
  log "Baseline: $baseline_count issues"
  log ""
  
  #-----------------------------------------
  # BENCHMARK PHASE: Edit → Rebuild → Measure
  #-----------------------------------------
  local failed=0
  for i in $(seq 1 "$ITERATIONS"); do
    if ! run_iteration "$i" "$baseline_file" "$baseline_count"; then
      log_error "Iteration $i failed"
      failed=1
      break
    fi
    echo ""
  done
  
  rm -f "$baseline_file"
  stop_server
  
  # Print timing summary
  if [[ $TIMING -eq 1 ]]; then
    echo ""
    log "⏱ Timing Summary:"
    local total=0
    # Count operations:
    # Warmup: 1 cold_reactive + 1 warm
    # Per iteration: 4 incremental, 4 cold, 4 rebuilds, 4 compares
    local num_incremental=$((4 * ITERATIONS))
    local num_standalone=$((4 * ITERATIONS))
    local num_rebuilds=$((4 * ITERATIONS))
    local num_compares=$((4 * ITERATIONS))
    
    for key in initial_build server_startup cold_reactive warm incremental standalone json_compare rebuild; do
      local val="${TIMING_TOTALS[$key]:-0}"
      total=$(python3 -c "print($total + $val)")
      
      case "$key" in
        cold_reactive)
          local avg=$(python3 -c "print(f'{$val * 1000:.0f}')")
          printf "  %-20s %6.2fs  (1 run, %sms) ← populates cache\n" "$key:" "$val" "$avg"
          ;;
        warm)
          local avg=$(python3 -c "print(f'{$val * 1000:.0f}')")
          printf "  %-20s %6.2fs  (1 run, %sms) ← verifies cache\n" "$key:" "$val" "$avg"
          ;;
        incremental)
          local avg=$(python3 -c "print(f'{$val / $num_incremental * 1000:.0f}')")
          printf "  %-20s %6.2fs  (%d runs, ~%sms each)\n" "$key:" "$val" "$num_incremental" "$avg"
          ;;
        standalone)
          local avg=$(python3 -c "print(f'{$val / $num_standalone * 1000:.0f}')")
          printf "  %-20s %6.2fs  (%d runs, ~%sms each)\n" "$key:" "$val" "$num_standalone" "$avg"
          ;;
        json_compare)
          local avg=$(python3 -c "print(f'{$val / $num_compares * 1000:.0f}')")
          printf "  %-20s %6.2fs  (%d compares, ~%sms each)\n" "$key:" "$val" "$num_compares" "$avg"
          ;;
        rebuild)
          local avg=$(python3 -c "print(f'{$val / $num_rebuilds * 1000:.0f}')")
          printf "  %-20s %6.2fs  (%d rebuilds, ~%sms each)\n" "$key:" "$val" "$num_rebuilds" "$avg"
          ;;
        *)
          printf "  %-20s %6.2fs\n" "$key:" "$val"
          ;;
      esac
    done
    printf "  %-20s %6.2fs\n" "TOTAL:" "$total"
    
    # Show speedup comparison: incremental vs standalone
    local incr_total="${TIMING_TOTALS[incremental]:-0}"
    local standalone_total="${TIMING_TOTALS[standalone]:-0}"
    if [[ $(python3 -c "print(1 if $incr_total > 0 and $num_incremental > 0 else 0)") -eq 1 ]]; then
      local incr_avg=$(python3 -c "print(f'{$incr_total / $num_incremental * 1000:.0f}')")
      local standalone_avg=$(python3 -c "print(f'{$standalone_total / $num_standalone * 1000:.0f}')")
      local speedup=$(python3 -c "print(f'{float($standalone_avg) / float($incr_avg):.1f}')")
      echo ""
      log "📊 Speedup: incremental ~${incr_avg}ms vs standalone ~${standalone_avg}ms = ${speedup}x faster"
    fi
  fi
  
  if [[ $failed -eq 0 ]]; then
    log "✅ All $ITERATIONS iterations passed"
    exit 0
  else
    log_error "❌ Test failed"
    exit 1
  fi
}

main

