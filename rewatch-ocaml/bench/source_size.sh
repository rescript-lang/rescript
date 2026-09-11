#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
cloc_command=${CLOC:-cloc}

if ! command -v "$cloc_command" >/dev/null 2>&1; then
  echo "source_size.sh requires cloc (or set CLOC to its executable)" >&2
  exit 1
fi

work_dir=$(mktemp -d)
trap 'rm -r "$work_dir"' EXIT

rust_production="$work_dir/rust-production"
rust_tests="$work_dir/rust-tests"
mkdir -p "$rust_production" "$rust_tests"
while IFS= read -r source; do
  relative=${source#"$repo_root/rewatch/src/"}
  destination="$rust_production/$relative"
  test_destination="$rust_tests/$relative"
  mkdir -p "$(dirname "$destination")"
  mkdir -p "$(dirname "$test_destination")"
  # Rust keeps unit tests beside production code. Stop at the first test-only
  # module so the production comparison matches OCaml's separate test package.
  awk '/^#\[cfg\(test\)\]/{exit} {print}' "$source" > "$destination"
  awk 'found || /^#\[cfg\(test\)\]/{found = 1; print}' "$source" \
    > "$test_destination"
done < <(find "$repo_root/rewatch/src" -type f -name '*.rs' \
  ! -name telemetry.rs | sort)

mapfile -t ocaml_production < <(find "$repo_root/rewatch-ocaml" -maxdepth 1 \
  -type f \( -name '*.ml' -o -name '*.mli' \) | sort)
mapfile -t ocaml_unit_tests < <(find "$repo_root/tests/rewatch_ounit_tests" \
  -maxdepth 1 -type f -name '*.ml' | sort)
mapfile -t ocaml_focused_test_relative < <(git -C "$repo_root" ls-files \
  rewatch-ocaml/tests | sort)
ocaml_focused_tests=()
for relative in "${ocaml_focused_test_relative[@]}"; do
  ocaml_focused_tests+=("$repo_root/$relative")
done
mapfile -t ocaml_benchmark_tooling < <(find "$repo_root/rewatch-ocaml/bench" \
  -maxdepth 1 -type f \( -name '*.sh' -o -name '*.js' \) | sort)

count() {
  local label=$1
  shift
  local totals
  totals=$("$cloc_command" --csv --quiet --skip-uniqueness "$@" \
    | awk -F, '$2 == "SUM" {print $3 "," $4 "," $5}')
  printf '%-34s %8s %8s %8s\n' "$label" \
    "${totals%%,*}" "$(cut -d, -f2 <<< "$totals")" "${totals##*,}"
}

printf '%-34s %8s %8s %8s\n' Scope Blank Comment Code
count "Rust production, no telemetry" "$rust_production"
count "Rust unit tests, no telemetry" "$rust_tests"
count "OCaml production" "${ocaml_production[@]}"
count "OCaml test code and fixtures" \
  --force-lang=ReScript,fixed --force-lang=ReScript,invalid \
  "${ocaml_unit_tests[@]}" "${ocaml_focused_tests[@]}"
count "OCaml benchmark tooling" "${ocaml_benchmark_tooling[@]}"

largest() {
  local label=$1
  shift
  printf '\n%s (code lines):\n' "$label"
  "$cloc_command" --by-file --csv --quiet --skip-uniqueness "$@" \
    | awk -F, -v root="$repo_root/" \
        '$1 != "language" && $1 != "SUM" {
          sub("^" root, "", $2);
          printf "%8d  %s\n", $5, $2
        }' \
    | sort -nr \
    | head -10
}

largest "Largest OCaml production modules" "${ocaml_production[@]}"
largest "Largest OCaml test/tooling files" \
  --force-lang=ReScript,fixed --force-lang=ReScript,invalid \
  "${ocaml_unit_tests[@]}" "${ocaml_focused_tests[@]}" \
  "${ocaml_benchmark_tooling[@]}"

printf '\ncloc version: %s\n' "$("$cloc_command" --version)"
