#!/usr/bin/env bash
# SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
# Hyperpolymath comprehensive test suite for ReScript v13.0.0-alpha.2
#
# Test categories:
#   - point_to_point: Unit tests for each compiler phase (lexer, parser,
#     frontend, typechecker, lambda, JS codegen, JS output, gentype,
#     runtime, analysis, rewatch, CLI)
#   - end_to_end: Full pipeline tests (compilation, error reporting,
#     module system, interop, optimization, incremental)
#   - aspect_oriented: Cross-cutting concern tests (error recovery,
#     source positions, warnings, performance, memory, determinism,
#     compatibility)
#   - benchmarks: Performance benchmarks (parse, typecheck, compile,
#     codegen, rewatch, stdlib)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colours
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

print_header() {
  echo -e "\n${CYAN}═══════════════════════════════════════════════════${NC}"
  echo -e "${CYAN} $1${NC}"
  echo -e "${CYAN}═══════════════════════════════════════════════════${NC}\n"
}

print_section() {
  echo -e "\n${YELLOW}── $1 ──${NC}\n"
}

# Parse arguments
RUN_ALL=true
RUN_P2P=false
RUN_E2E=false
RUN_ASPECT=false
RUN_BENCH=false

for arg in "$@"; do
  case "$arg" in
    -p2p|--point-to-point) RUN_P2P=true; RUN_ALL=false ;;
    -e2e|--end-to-end) RUN_E2E=true; RUN_ALL=false ;;
    -ao|--aspect-oriented) RUN_ASPECT=true; RUN_ALL=false ;;
    -bench|--benchmarks) RUN_BENCH=true; RUN_ALL=false ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  -p2p, --point-to-point   Run point-to-point tests only"
      echo "  -e2e, --end-to-end       Run end-to-end tests only"
      echo "  -ao,  --aspect-oriented  Run aspect-oriented tests only"
      echo "  -bench, --benchmarks     Run benchmarks only"
      echo "  -h,  --help              Show this help"
      echo ""
      echo "With no options, all test categories are run."
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $arg${NC}" >&2
      exit 1
      ;;
  esac
done

ERRORS=0
PASSED=0
TOTAL=0

run_test_category() {
  local category="$1"
  local label="$2"
  print_section "$label"

  local files
  files=$(find "$SCRIPT_DIR/$category" -name "*_test.mjs" -type f 2>/dev/null | sort)
  if [ -z "$files" ]; then
    echo -e "${YELLOW}  No compiled test files found in $category${NC}"
    return
  fi

  for f in $files; do
    local name
    name=$(basename "$f" .mjs)
    TOTAL=$((TOTAL + 1))
    if node "$f" 2>&1; then
      PASSED=$((PASSED + 1))
      echo -e "  ${GREEN}✓${NC} $name"
    else
      ERRORS=$((ERRORS + 1))
      echo -e "  ${RED}✗${NC} $name"
    fi
  done
}

# Build the test suite
print_header "Hyperpolymath ReScript Test Suite"
echo "ReScript v13.0.0-alpha.2 — Comprehensive lifecycle testing"
echo ""

print_section "Building test suite"
cd "$SCRIPT_DIR"

# Use the project-local rescript binary
RESCRIPT="$PROJECT_ROOT/cli/rescript.js"
if [ ! -f "$RESCRIPT" ]; then
  echo -e "${RED}Error: rescript CLI not found at $RESCRIPT${NC}"
  echo "Run 'make lib' from the project root first."
  exit 1
fi

node "$RESCRIPT" build 2>&1 || {
  echo -e "${RED}Build failed!${NC}"
  exit 1
}
echo -e "${GREEN}Build succeeded${NC}"

# Run selected categories
if $RUN_ALL || $RUN_P2P; then
  print_header "Point-to-Point Tests"
  run_test_category "point_to_point/01_lexer"       "Phase 1: Lexer/Scanner"
  run_test_category "point_to_point/02_parser"      "Phase 2: Parser"
  run_test_category "point_to_point/03_frontend"    "Phase 3: Frontend Transforms"
  run_test_category "point_to_point/04_typechecker" "Phase 4: Type Checker"
  run_test_category "point_to_point/05_lambda"      "Phase 5: Lambda IR"
  run_test_category "point_to_point/06_js_codegen"  "Phase 6: JS Code Generation"
  run_test_category "point_to_point/07_js_output"   "Phase 7: JS Output"
  run_test_category "point_to_point/08_gentype"     "Phase 8: GenType"
  run_test_category "point_to_point/09_runtime"     "Phase 9: Runtime/Stdlib"
  run_test_category "point_to_point/10_analysis"    "Phase 10: Analysis/LSP"
  run_test_category "point_to_point/11_rewatch"     "Phase 11: Rewatch"
  run_test_category "point_to_point/12_cli"         "Phase 12: CLI"
fi

if $RUN_ALL || $RUN_E2E; then
  print_header "End-to-End Tests"
  run_test_category "end_to_end/compilation"     "Full Pipeline Compilation"
  run_test_category "end_to_end/error_reporting" "Error Reporting"
  run_test_category "end_to_end/module_system"   "Module System"
  run_test_category "end_to_end/interop"         "JS Interop"
  run_test_category "end_to_end/optimization"    "Optimization Pipeline"
  run_test_category "end_to_end/incremental"     "Incremental Compilation"
fi

if $RUN_ALL || $RUN_ASPECT; then
  print_header "Aspect-Oriented Tests"
  run_test_category "aspect_oriented/error_recovery"   "Error Recovery"
  run_test_category "aspect_oriented/source_positions" "Source Position Tracking"
  run_test_category "aspect_oriented/warnings"         "Warning System"
  run_test_category "aspect_oriented/performance"      "Performance Patterns"
  run_test_category "aspect_oriented/memory"           "Memory Patterns"
  run_test_category "aspect_oriented/determinism"      "Output Determinism"
  run_test_category "aspect_oriented/compatibility"    "Backward Compatibility"
fi

if $RUN_ALL || $RUN_BENCH; then
  print_header "Benchmarks"
  run_test_category "benchmarks/parse"     "Parser Benchmarks"
  run_test_category "benchmarks/typecheck" "Type Checker Benchmarks"
  run_test_category "benchmarks/compile"   "Compilation Benchmarks"
  run_test_category "benchmarks/codegen"   "Code Generation Benchmarks"
  run_test_category "benchmarks/stdlib"    "Stdlib Benchmarks"
  run_test_category "benchmarks/rewatch"   "Rewatch Benchmarks"
fi

# Summary
print_header "Test Summary"
echo -e "  Total:  ${TOTAL}"
echo -e "  ${GREEN}Passed: ${PASSED}${NC}"
if [ "$ERRORS" -gt 0 ]; then
  echo -e "  ${RED}Failed: ${ERRORS}${NC}"
  exit 1
else
  echo -e "  ${RED}Failed: 0${NC}"
  echo -e "\n${GREEN}All tests passed!${NC}"
fi
