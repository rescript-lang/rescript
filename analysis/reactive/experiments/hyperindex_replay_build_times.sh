#!/bin/zsh
set -euo pipefail

# Fixed benchmark range/repo:
START_REF="benchmark/rescript-baseline"
END_REF="benchmark/rescript-followup"
HYPERINDEX_REPO="/Users/cristianocalcagno/GitHub/hyperindex"
OUT_DIR="${OUT_DIR:-/tmp/hyperindex-replay-times-refs}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESCRIPT_REPO="$(cd "$SCRIPT_DIR/../../.." && pwd)"
TOOLS_BIN="$RESCRIPT_REPO/_build/default/tools/bin/main.exe"
SOCKET_FILE="$HYPERINDEX_REPO/.rescript-reanalyze.sock"
SERVER_LOG="$OUT_DIR/reactive-server.log"
SERVER_PID=""
FIXPOINT_ASSERT="${FIXPOINT_ASSERT:-1}"
FIXPOINT_METRICS="${FIXPOINT_METRICS:-1}"

mkdir -p "$OUT_DIR"

cd "$HYPERINDEX_REPO"
ORIG_REF="$(git rev-parse --abbrev-ref HEAD)"

stop_server() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    # Ask server to flush fixpoint metrics before shutdown.
    kill -USR1 "$SERVER_PID" 2>/dev/null || true
    sleep 0.2
    # Prefer SIGINT so OCaml runtime has a chance to run at_exit hooks.
    kill -INT "$SERVER_PID" 2>/dev/null || true
    for _ in {1..30}; do
      if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        break
      fi
      sleep 0.1
    done
    if kill -0 "$SERVER_PID" 2>/dev/null; then
      kill -TERM "$SERVER_PID" 2>/dev/null || true
    fi
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  rm -f "$SOCKET_FILE" 2>/dev/null || true
}

cleanup() {
  stop_server
  git checkout -q "$ORIG_REF" >/dev/null 2>&1 || true
}
trap cleanup EXIT

if [[ ! -x "$TOOLS_BIN" ]]; then
  echo "missing tools binary: $TOOLS_BIN" >&2
  echo "build it from rescript repo root with: make" >&2
  exit 1
fi

if ! git rev-parse --verify "$START_REF" >/dev/null 2>&1; then
  echo "missing ref: $START_REF" >&2
  exit 1
fi
if ! git rev-parse --verify "$END_REF" >/dev/null 2>&1; then
  echo "missing ref: $END_REF" >&2
  exit 1
fi

COMMITS=(${(@f)$(git rev-list --first-parent --reverse "$START_REF..$END_REF")})
TOTAL=${#COMMITS[@]}

SUMMARY="$OUT_DIR/summary.tsv"
echo -e "idx\tcommit\tbuild_status\tbuild_real_seconds\treactive_status\treactive_real_seconds\treactive_issue_count\tcold_status\tcold_real_seconds\treactive_vs_cold_pct\tchanged_files\tinsertions\tdeletions" > "$SUMMARY"

echo "Starting reactive server with debug assertions..."
rm -f "$SOCKET_FILE" "$SERVER_LOG"
RESCRIPT_REACTIVE_FIXPOINT_ASSERT="$FIXPOINT_ASSERT" \
RESCRIPT_REACTIVE_FIXPOINT_METRICS="$FIXPOINT_METRICS" \
"$TOOLS_BIN" reanalyze-server >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

for _ in {1..60}; do
  if [[ -S "$SOCKET_FILE" ]]; then
    break
  fi
  sleep 0.1
done

if [[ ! -S "$SOCKET_FILE" ]]; then
  echo "reactive server failed to start; log follows:" >&2
  cat "$SERVER_LOG" >&2
  exit 1
fi

echo "Replay start: $START_REF..$END_REF ($TOTAL commits)"

idx=0
for c in $COMMITS; do
  idx=$((idx+1))
  echo "[$idx/$TOTAL] $c"

  git checkout -q "$c"

  BUILD_LOG="$OUT_DIR/${idx}_${c}.build.log"
  TIME_LOG="$OUT_DIR/${idx}_${c}.time.log"
  REACTIVE_JSON="$OUT_DIR/${idx}_${c}.reactive.json"
  REACTIVE_TIME_LOG="$OUT_DIR/${idx}_${c}.reactive.time.log"
  REACTIVE_STDERR="$OUT_DIR/${idx}_${c}.reactive.stderr.log"
  COLD_JSON="$OUT_DIR/${idx}_${c}.cold.json"
  COLD_TIME_LOG="$OUT_DIR/${idx}_${c}.cold.time.log"

  set +e
  change_stats="$(git show --numstat --format='' "$c" | awk '
    BEGIN {files=0; ins=0; del=0}
    NF==3 {
      files++
      if ($1 != "-") ins += $1
      if ($2 != "-") del += $2
    }
    END {printf "%d %d %d", files, ins, del}
  ')"
  change_stats_rc=$?
  set -e
  if [[ $change_stats_rc -ne 0 || -z "${change_stats:-}" ]]; then
    changed_files="NA"
    insertions="NA"
    deletions="NA"
  else
    changed_files="$(echo "$change_stats" | awk '{print $1}')"
    insertions="$(echo "$change_stats" | awk '{print $2}')"
    deletions="$(echo "$change_stats" | awk '{print $3}')"
  fi

  set +e
  /usr/bin/time -p pnpm exec rescript >"$BUILD_LOG" 2>"$TIME_LOG"
  build_rc=$?
  set -e

  build_real="$(awk '/^real / {print $2}' "$TIME_LOG" | tail -n 1)"
  [[ -z "${build_real:-}" ]] && build_real="NA"

  if [[ $build_rc -eq 0 ]]; then
    build_status="ok"
  else
    build_status="fail($build_rc)"
  fi

  if [[ "$build_status" != "ok" ]]; then
    echo -e "${idx}\t${c}\t${build_status}\t${build_real}\tskipped\tNA\tNA\tskipped\tNA\tNA\t${changed_files}\t${insertions}\t${deletions}" >> "$SUMMARY"
    echo "stop: commit $c build_status=$build_status" >&2
    exit 2
  fi

  set +e
  /usr/bin/time -p "$TOOLS_BIN" reanalyze -json >"$REACTIVE_JSON" 2>"$REACTIVE_TIME_LOG"
  reactive_rc=$?
  set -e

  reactive_real="$(awk '/^real / {print $2}' "$REACTIVE_TIME_LOG" | tail -n 1)"
  [[ -z "${reactive_real:-}" ]] && reactive_real="NA"

  if [[ $reactive_rc -eq 0 ]]; then
    reactive_status="ok"
    reactive_issues="$(python3 -c "import json; print(len(json.load(open('$REACTIVE_JSON'))))" 2>"$REACTIVE_STDERR" || true)"
    [[ -z "${reactive_issues:-}" ]] && reactive_issues="NA"
  else
    reactive_status="fail($reactive_rc)"
    reactive_issues="NA"
  fi

  if [[ "$reactive_status" != "ok" ]]; then
    echo -e "${idx}\t${c}\t${build_status}\t${build_real}\t${reactive_status}\t${reactive_real}\t${reactive_issues}\tskipped\tNA\tNA\t${changed_files}\t${insertions}\t${deletions}" >> "$SUMMARY"
    echo "stop: commit $c reactive_status=$reactive_status" >&2
    echo "server log: $SERVER_LOG" >&2
    exit 3
  fi

  set +e
  /usr/bin/time -p env RESCRIPT_REANALYZE_NO_SERVER=1 "$TOOLS_BIN" reanalyze -json >"$COLD_JSON" 2>"$COLD_TIME_LOG"
  cold_rc=$?
  set -e

  cold_real="$(awk '/^real / {print $2}' "$COLD_TIME_LOG" | tail -n 1)"
  [[ -z "${cold_real:-}" ]] && cold_real="NA"

  if [[ $cold_rc -eq 0 ]]; then
    cold_status="ok"
  else
    cold_status="fail($cold_rc)"
  fi

  if [[ "$cold_status" == "ok" && "$reactive_real" != "NA" && "$cold_real" != "NA" ]]; then
    reactive_vs_cold_pct="$(awk -v r="$reactive_real" -v c="$cold_real" 'BEGIN { if (c == 0) print "0.00"; else printf "%.2f", (100.0 * r / c) }')"
  else
    reactive_vs_cold_pct="NA"
  fi

  echo -e "${idx}\t${c}\t${build_status}\t${build_real}\t${reactive_status}\t${reactive_real}\t${reactive_issues}\t${cold_status}\t${cold_real}\t${reactive_vs_cold_pct}\t${changed_files}\t${insertions}\t${deletions}" >> "$SUMMARY"

  if [[ "$cold_status" != "ok" ]]; then
    echo "stop: commit $c cold_status=$cold_status" >&2
    exit 4
  fi
done

stop_server

awk -F '\t' \
  -v start_ref="$START_REF" \
  -v end_ref="$END_REF" \
  '
   function update_num(x, kind,   v) {
     if (x == "NA" || x == "") return
     v = x + 0
     if (kind == "build") {
       build_sum += v
       if (build_min == "" || v < build_min) build_min = v
       if (build_max == "" || v > build_max) build_max = v
       build_n++
     } else if (kind == "reactive") {
       reactive_sum += v
       if (reactive_min == "" || v < reactive_min) reactive_min = v
       if (reactive_max == "" || v > reactive_max) reactive_max = v
       reactive_n++
     } else if (kind == "issues") {
       issues_sum += v
       issues_n++
     } else if (kind == "cold") {
       cold_sum += v
       if (cold_min == "" || v < cold_min) cold_min = v
       if (cold_max == "" || v > cold_max) cold_max = v
       cold_n++
     } else if (kind == "ratio") {
       ratio_sum += v
       ratio_n++
     } else if (kind == "files") {
       files_sum += v
       files_n++
     } else if (kind == "ins") {
       ins_sum += v
       ins_n++
     } else if (kind == "del") {
       del_sum += v
       del_n++
     }
   }
   NR==1 {next}
   {
     if ($3=="ok") {
       build_ok++
       update_num($4, "build")
     } else {
       build_fail++
     }

     if ($5=="ok") {
       reactive_ok++
       update_num($6, "reactive")
       update_num($7, "issues")
     } else if ($5=="skipped") {
       reactive_skipped++
     } else {
       reactive_fail++
     }

     if ($8=="ok") {
       cold_ok++
       update_num($9, "cold")
     } else if ($8=="skipped") {
       cold_skipped++
     } else {
       cold_fail++
     }

     update_num($10, "ratio")
     update_num($11, "files")
     update_num($12, "ins")
     update_num($13, "del")
   }
   END {
     printf "START=%s END=%s TOTAL=%d\n", start_ref, end_ref, NR-1
     printf "BUILD_OK=%d BUILD_FAIL=%d BUILD_AVG_OK=%.3f BUILD_MIN_OK=%.3f BUILD_MAX_OK=%.3f\n",
       build_ok, build_fail, (build_n?build_sum/build_n:0), (build_n?build_min:0), (build_n?build_max:0)
     printf "REACTIVE_OK=%d REACTIVE_FAIL=%d REACTIVE_SKIPPED=%d REACTIVE_AVG_OK=%.3f REACTIVE_MIN_OK=%.3f REACTIVE_MAX_OK=%.3f REACTIVE_AVG_ISSUES=%.2f\n",
       reactive_ok, reactive_fail, reactive_skipped, (reactive_n?reactive_sum/reactive_n:0), (reactive_n?reactive_min:0), (reactive_n?reactive_max:0), (issues_n?issues_sum/issues_n:0)
     printf "COLD_OK=%d COLD_FAIL=%d COLD_SKIPPED=%d COLD_AVG_OK=%.3f COLD_MIN_OK=%.3f COLD_MAX_OK=%.3f\n",
       cold_ok, cold_fail, cold_skipped, (cold_n?cold_sum/cold_n:0), (cold_n?cold_min:0), (cold_n?cold_max:0)
     printf "REACTIVE_VS_COLD_PCT_AVG=%.2f\n", (ratio_n?ratio_sum/ratio_n:0)
     printf "CHANGE_STATS_AVG_FILES=%.2f CHANGE_STATS_AVG_INSERTIONS=%.2f CHANGE_STATS_AVG_DELETIONS=%.2f CHANGE_STATS_TOTAL_INSERTIONS=%.0f CHANGE_STATS_TOTAL_DELETIONS=%.0f\n",
       (files_n?files_sum/files_n:0), (ins_n?ins_sum/ins_n:0), (del_n?del_sum/del_n:0), ins_sum, del_sum
   }' "$SUMMARY" > "$OUT_DIR/stats.txt"

METRICS_SUMMARY="$( (grep -F '[ReactiveFixpointMetrics]' "$SERVER_LOG" || true) | tail -n 1 | sed -E 's/^.*\[ReactiveFixpointMetrics\] //')"
if [[ -n "${METRICS_SUMMARY:-}" ]]; then
  echo "$METRICS_SUMMARY" > "$OUT_DIR/reactive_fixpoint_metrics.txt"
  echo "REACTIVE_FIXPOINT_METRICS=$METRICS_SUMMARY" >> "$OUT_DIR/stats.txt"
fi

echo
echo "done: $OUT_DIR"
echo "reactive server log: $SERVER_LOG"
if [[ -n "${METRICS_SUMMARY:-}" ]]; then
  echo "reactive fixpoint metrics: $OUT_DIR/reactive_fixpoint_metrics.txt"
fi
cat "$OUT_DIR/stats.txt"
