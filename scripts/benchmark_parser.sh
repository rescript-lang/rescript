#!/bin/bash

# Performance benchmark comparing Rust and OCaml parsers
# Uses roundtrip test files from tests/syntax_tests/data/{idempotency,printer}

set -e

scriptDir=$(dirname "$0")
PROJECT_ROOT=$(cd "$scriptDir/.."; pwd -P)
DUNE_BIN_DIR="$PROJECT_ROOT/_build/install/default/bin"
RUST_PARSER="$PROJECT_ROOT/compiler-rust/target/release/res_parser_rust"
OCAML_PARSER="$DUNE_BIN_DIR/res_parser"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

# Configuration
WARMUP_RUNS=1
BENCHMARK_RUNS=3
TIMEOUT_SECONDS=5

# Check if parsers exist
if [[ ! -x "$OCAML_PARSER" ]]; then
    echo -e "${RED}Error: OCaml parser not found at $OCAML_PARSER${RESET}"
    echo "Run 'make' first to build the OCaml parser."
    exit 1
fi

if [[ ! -x "$RUST_PARSER" ]]; then
    echo -e "${RED}Error: Rust parser not found at $RUST_PARSER${RESET}"
    echo "Run 'cargo build --manifest-path compiler-rust/Cargo.toml --release' first."
    exit 1
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo -e "${BOLD}Parser Performance Benchmark${RESET}"
echo "============================================"
echo "OCaml parser: $OCAML_PARSER"
echo "Rust parser:  $RUST_PARSER"
echo "Warmup runs:  $WARMUP_RUNS"
echo "Benchmark runs: $BENCHMARK_RUNS"
echo "Timeout: ${TIMEOUT_SECONDS}s"
echo ""

# Find all test files
cd "$PROJECT_ROOT/tests"
find syntax_tests/data/idempotency syntax_tests/data/printer -name "*.res" -o -name "*.resi" 2>/dev/null | sort > "$TEMP_DIR/files.txt"
TOTAL_FILES=$(wc -l < "$TEMP_DIR/files.txt" | tr -d ' ')

echo -e "Found ${BOLD}$TOTAL_FILES${RESET} test files"
echo ""

# Results files
RESULTS_FILE="$TEMP_DIR/results.csv"
echo "file,ocaml_us,rust_us,status" > "$RESULTS_FILE"

# Function to measure execution time in microseconds using python
# OCaml parser uses: res_parser -print res file
# Rust parser uses: res_parser_rust --print res file (or -p res file)
measure_time_us() {
    local parser="$1"
    local parser_type="$2"  # "ocaml" or "rust"
    local file="$3"
    local intf_flag="$4"
    local runs="$5"

    python3 << PYTHON
import subprocess
import time
import sys

parser = "$parser"
parser_type = "$parser_type"
file = "$file"
intf_flag = "$intf_flag".strip()
runs = $runs
timeout = $TIMEOUT_SECONDS

total_us = 0
for i in range(runs):
    cmd = [parser]
    if intf_flag:
        if parser_type == "ocaml":
            cmd.append(intf_flag)  # -interface
        else:
            cmd.append("--interface")  # Rust uses long form

    # Print format differs between parsers
    if parser_type == "ocaml":
        cmd.extend(["-print", "res", file])
    else:
        cmd.extend(["--print", "res", file])

    start = time.perf_counter()
    try:
        result = subprocess.run(cmd, capture_output=True, timeout=timeout)
        end = time.perf_counter()
        if result.returncode != 0:
            # Print error for debugging (on first failure only)
            if i == 0:
                import sys
                print(f"FAIL:{result.stderr.decode()[:200]}", file=sys.stderr)
            print("FAIL")
            sys.exit(0)
        elapsed_us = int((end - start) * 1_000_000)
        total_us += elapsed_us
    except subprocess.TimeoutExpired:
        print("TIMEOUT")
        sys.exit(0)
    except Exception as e:
        print("FAIL")
        sys.exit(0)

avg_us = total_us // runs
print(avg_us)
PYTHON
}

echo -e "${BOLD}Running benchmark...${RESET}"
echo ""

file_index=0
total_ocaml_us=0
total_rust_us=0
comparable_count=0
rust_faster_count=0
rust_slower_count=0
rust_fail_count=0
ocaml_fail_count=0

slower_files=""
failed_files=""

while read -r file; do
    file_index=$((file_index + 1))

    # Progress indicator
    if [[ $((file_index % 50)) -eq 0 ]]; then
        echo "  Progress: $file_index / $TOTAL_FILES files..."
    fi

    # Determine if it's an interface file
    intf_flag=""
    case "$file" in
        *.resi) intf_flag="-interface" ;;
    esac

    # Warmup runs (discard results)
    measure_time_us "$OCAML_PARSER" "ocaml" "$file" "$intf_flag" "$WARMUP_RUNS" > /dev/null 2>&1 || true
    measure_time_us "$RUST_PARSER" "rust" "$file" "$intf_flag" "$WARMUP_RUNS" > /dev/null 2>&1 || true

    # Benchmark runs
    ocaml_result=$(measure_time_us "$OCAML_PARSER" "ocaml" "$file" "$intf_flag" "$BENCHMARK_RUNS" 2>/dev/null)
    rust_result=$(measure_time_us "$RUST_PARSER" "rust" "$file" "$intf_flag" "$BENCHMARK_RUNS" 2>/dev/null)

    # Record results
    status="ok"
    if [[ "$ocaml_result" == "FAIL" || "$ocaml_result" == "TIMEOUT" ]]; then
        ocaml_fail_count=$((ocaml_fail_count + 1))
        status="ocaml_fail"
    fi

    if [[ "$rust_result" == "FAIL" || "$rust_result" == "TIMEOUT" ]]; then
        rust_fail_count=$((rust_fail_count + 1))
        status="rust_fail"
        failed_files="$failed_files$file\n"
    fi

    if [[ "$ocaml_result" != "FAIL" && "$ocaml_result" != "TIMEOUT" && \
          "$rust_result" != "FAIL" && "$rust_result" != "TIMEOUT" ]]; then
        total_ocaml_us=$((total_ocaml_us + ocaml_result))
        total_rust_us=$((total_rust_us + rust_result))
        comparable_count=$((comparable_count + 1))

        if [[ "$rust_result" -le "$ocaml_result" ]]; then
            rust_faster_count=$((rust_faster_count + 1))
        else
            rust_slower_count=$((rust_slower_count + 1))
            # Convert to ms for readability
            ocaml_ms=$((ocaml_result / 1000))
            rust_ms=$((rust_result / 1000))
            slower_files="$slower_files$rust_ms $ocaml_ms $file\n"
        fi
    fi

    echo "$file,$ocaml_result,$rust_result,$status" >> "$RESULTS_FILE"

done < "$TEMP_DIR/files.txt"

echo ""
echo ""
echo -e "${BOLD}============================================${RESET}"
echo -e "${BOLD}RESULTS${RESET}"
echo -e "${BOLD}============================================${RESET}"
echo ""

echo -e "${BOLD}Summary:${RESET}"
echo "  Total files:                $TOTAL_FILES"
echo "  Files where both succeeded: $comparable_count"
echo "  OCaml parse failures:       $ocaml_fail_count"
echo "  Rust parse failures:        $rust_fail_count"
echo ""

if [[ $comparable_count -gt 0 ]]; then
    # Convert to milliseconds for display
    total_ocaml_ms=$((total_ocaml_us / 1000))
    total_rust_ms=$((total_rust_us / 1000))

    echo -e "${BOLD}Timing (files where both parsers succeeded):${RESET}"
    echo "  Total OCaml time: ${total_ocaml_ms}ms"
    echo "  Total Rust time:  ${total_rust_ms}ms"

    if [[ $total_rust_us -gt 0 && $total_ocaml_us -gt 0 ]]; then
        # Calculate speedup with python for floating point
        overall_speedup=$(python3 -c "print(f'{$total_ocaml_us / $total_rust_us:.2f}')")
        echo ""
        # Check if speedup > 1
        is_faster=$(python3 -c "print('yes' if $total_ocaml_us > $total_rust_us else 'no')")
        if [[ "$is_faster" == "yes" ]]; then
            echo -e "  ${GREEN}${BOLD}Overall: Rust parser is ${overall_speedup}x faster${RESET}"
        else
            slowdown=$(python3 -c "print(f'{$total_rust_us / $total_ocaml_us:.2f}')")
            echo -e "  ${RED}${BOLD}Overall: Rust parser is ${slowdown}x slower${RESET}"
        fi
    fi

    echo ""
    echo -e "${BOLD}Per-file comparison:${RESET}"
    echo "  Files where Rust >= OCaml speed: $rust_faster_count"
    echo "  Files where Rust < OCaml speed:  $rust_slower_count"
fi

# Show files where Rust is slower
if [[ $rust_slower_count -gt 0 ]]; then
    echo ""
    echo -e "${BOLD}${YELLOW}Top 20 files where Rust parser is slower:${RESET}"
    echo -e "$slower_files" | grep -v "^$" | sort -rn | head -20 | while read rust_ms ocaml_ms file; do
        if [[ -n "$rust_ms" && -n "$ocaml_ms" && "$ocaml_ms" != "0" ]]; then
            ratio=$(python3 -c "print(f'{$rust_ms / max($ocaml_ms, 1):.2f}')")
            printf "  Rust: %4dms, OCaml: %4dms (${ratio}x slower): %s\n" "$rust_ms" "$ocaml_ms" "$file"
        fi
    done
fi

# Show Rust parse failures (only if reasonable count)
if [[ $rust_fail_count -gt 0 ]]; then
    echo ""
    echo -e "${BOLD}${RED}Files where Rust parser failed: $rust_fail_count${RESET}"
    if [[ $rust_fail_count -le 30 ]]; then
        echo -e "$failed_files" | grep -v "^$" | head -20 | while read file; do
            echo "  $file"
        done
        if [[ $rust_fail_count -gt 20 ]]; then
            remaining=$((rust_fail_count - 20))
            echo "  ... and $remaining more"
        fi
    else
        echo "  (Too many to list - $rust_fail_count failures)"
    fi
fi

echo ""
echo -e "${BOLD}============================================${RESET}"
if [[ $rust_slower_count -eq 0 && $comparable_count -gt 0 ]]; then
    echo -e "${GREEN}${BOLD}SUCCESS: Rust parser is at least as fast as OCaml for all $comparable_count comparable files!${RESET}"
    exit 0
elif [[ $comparable_count -eq 0 ]]; then
    echo -e "${RED}${BOLD}ERROR: No files could be compared (all had parse failures)${RESET}"
    exit 1
else
    pct_slower=$(python3 -c "print(f'{100 * $rust_slower_count / $comparable_count:.1f}')")
    echo -e "${YELLOW}${BOLD}NOTE: Rust parser is slower for $rust_slower_count/$comparable_count files ($pct_slower%)${RESET}"
    exit 0
fi
