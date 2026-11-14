#!/bin/sh
# Pipelining (README "In-process compilation"): a module may start before its
# dependencies finish. Builds of a deep dependency chain must be the same with
# one compiler domain, where nothing starts early, and with several, where
# dependents start early; also when a dependency fails in code generation
# after it released its interface.
set -eu

. "$(dirname "$0")/test_helpers.sh"
port="$1"
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
native_path() {
  if $windows_posix_shell; then cygpath -am "$1"; else printf '%s\n' "$1"; fi
}
: "${RESCRIPT_RUNTIME:=$root/packages/@rescript/runtime}"
export RESCRIPT_RUNTIME
work="$root/tmp/rewatch-ocaml/pipelining-$$"
project="$work/chain"
mkdir -p "$project/src"
trap 'rm -rf "$work"' EXIT

cat >"$project/rescript.json" <<'EOF'
{
  "name": "pipelining-chain",
  "sources": {"dir": "src"},
  "package-specs": {"module": "esmodule", "in-source": true},
  "suffix": ".res.js"
}
EOF
printf 'let base = 1\nlet step = x => x + 1\nlet k = 42\ntype t = {a: int, b: string}\n' \
  >"$project/src/M0.res"
index=1
while [ "$index" -le 12 ]; do
  previous="M$((index - 1))"
  {
    printf 'let base = %s.base + %s.step(%d)\n' "$previous" "$previous" "$index"
    printf 'let step = x => %s.step(x) * 2\n' "$previous"
    printf 'let k = %s.k\ntype t = %s.t\n' "$previous" "$previous"
    printf 'let v: t = {a: %s.k, b: "%d"}\n' "$previous" "$index"
    function=0
    while [ "$function" -lt 40 ]; do
      printf 'let f%d = (x: int) => switch x {\n| 0 => %s.base\n| 1 => step(x) + %d\n| _ => k + x\n}\n' \
        "$function" "$previous" "$function"
      function=$((function + 1))
    done
  } >"$project/src/M$index.res"
  index=$((index + 1))
done
printf 'let r: int => int\n' >"$project/src/R.resi"
printf 'let r = x => M6.step(x) + M6.k\n' >"$project/src/R.res"
printf 'let u = R.r(3) + M12.base\n' >"$project/src/UseR.res"

snapshot() {
  # $1: output file. Compiled artifacts and generated JavaScript, by content.
  (
    cd "$project"
    find src lib/rescript -type f \
      \( -name '*.js' -o -name '*.cmi' -o -name '*.cmj' -o -name '*.cmt' \
      -o -name '*.cmti' -o -name '*.ast' -o -name '*.iast' \) 2>/dev/null \
      | LC_ALL=C sort | while IFS= read -r file; do
        printf '%s %s\n' "$(cksum <"$file")" "$file"
      done
  ) >"$1"
}

build_with() {
  # $1: domains, $2: label. Clean build; keeps output, log and snapshot.
  "$port" clean "$project" >/dev/null 2>&1
  status=0
  REWATCH_COMPILER_DOMAINS="$1" \
    REWATCH_SPECULATION_LOG="$(native_path "$work/$2.speculation")" \
    "$port" build --no-timing "$project" >"$work/$2.output" 2>&1 || status=$?
  echo "exit $status" >>"$work/$2.output"
  sed -e '/^#Start/d' -e '/^#Done/d' "$project/lib/rescript/.compiler.log" \
    >"$work/$2.log"
  snapshot "$work/$2.artifacts"
}

compare_builds() {
  # $1: label, $2: the speculation event expected in some parallel build.
  # One domain against four. Whether a dependent starts early depends on
  # timing, so the parallel build is repeated until the event shows; every
  # repetition must match the single-domain build.
  build_with 1 "$1-single"
  if [ -s "$work/$1-single.speculation" ]; then
    echo "a single-domain $1 build started a module early" >&2
    exit 1
  fi
  attempt=1
  seen=false
  while [ "$attempt" -le 5 ] && ! $seen; do
    build_with 4 "$1-parallel"
    for kind in output log artifacts; do
      if ! cmp -s "$work/$1-single.$kind" "$work/$1-parallel.$kind"; then
        echo "pipelining changed the $kind of the $1 build" >&2
        diff "$work/$1-single.$kind" "$work/$1-parallel.$kind" >&2 || true
        exit 1
      fi
    done
    if grep "^$2" "$work/$1-parallel.speculation" >/dev/null 2>&1; then
      seen=true
    fi
    attempt=$((attempt + 1))
  done
  if ! $seen; then
    # Covered deterministically by the scheduler's unit tests.
    echo "note: no parallel $1 build logged $2 (timing); outputs matched" >&2
  fi
}

compare_builds success adopt
grep 'exit 0' "$work/success-single.output" >/dev/null

# M3 announces its interface, then fails in code generation; its dependents
# that started early are abandoned without output or artifacts.
{
  function=0
  while [ "$function" -lt 300 ]; do
    printf 'let g%d = (x: int) => switch x { | 0 => base | _ => step(x) + %d }\n' \
      "$function" "$function"
    function=$((function + 1))
  done
  printf 'external has: (dict<int>, string, int) => bool = "%%dict_has"\n'
  printf 'let h = has(dict{}, "a", 1)\n'
} >>"$project/src/M3.res"
compare_builds codegen-failure abandon
grep 'exit 1' "$work/codegen-failure-single.output" >/dev/null
grep 'dict_has' "$work/codegen-failure-single.log" >/dev/null
