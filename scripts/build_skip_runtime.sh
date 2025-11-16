#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST_DIR="$PROJECT_ROOT/analysis/bin/skip_runtime"
VENDOR_DIR="$PROJECT_ROOT/analysis/skip_runtime/vendor"

mkdir -p "$DEST_DIR"

WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/skip-runtime.XXXXXX")"
cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

OCAML_LIB_DIR="$(ocamlc -where)"
OCAML_VERSION="$(ocamlc -version)"
OCAML_MAJOR="${OCAML_VERSION%%.*}"

CFLAGS=(-O2 -g "-I${OCAML_LIB_DIR}" "-I${VENDOR_DIR}/src/c" "-I${VENDOR_DIR}/external/runtime" -DSKIP64 -Wno-c2x-extensions -Wno-extern-c-compat -DRELEASE -DSKIP_NO_MAIN)
if [ "$OCAML_MAJOR" -ge 5 ]; then
  CFLAGS+=(-DOCAML5)
fi

compile_c() {
  local src="$1"
  local out="$2"
  clang -g -c "${CFLAGS[@]}" "$src" -o "$out"
}

compile_cpp() {
  local src="$1"
  local out="$2"
  clang++ -g -c "${CFLAGS[@]}" "$src" -o "$out"
}

compile_llvm() {
  local src="$1"
  local out="$2"
  clang++ -g -Wno-override-module -c "$src" -o "$out"
}

maybe_strip_main() {
  local obj="$1"
  if command -v objcopy >/dev/null 2>&1; then
    objcopy --strip-symbol=main "$obj" || true
  elif command -v llvm-objcopy >/dev/null 2>&1; then
    llvm-objcopy --strip-symbol=main "$obj" || true
  fi
}

shopt -s nullglob
OBJECTS=()

for src in "$VENDOR_DIR"/src/c/*.c; do
  obj="$WORK_DIR/c_$(basename "${src%.c}").o"
  compile_c "$src" "$obj"
  OBJECTS+=("$obj")
done

for src in "$VENDOR_DIR"/src/c/*.cpp; do
  obj="$WORK_DIR/cpp_$(basename "${src%.cpp}").o"
  compile_cpp "$src" "$obj"
  OBJECTS+=("$obj")
done

for src in "$VENDOR_DIR"/external/runtime/*.c; do
  if [[ "$(basename "$src")" == "runtime32_specific.c" ]]; then
    continue
  fi
  obj="$WORK_DIR/runtime_$(basename "${src%.c}").o"
  compile_c "$src" "$obj"
  OBJECTS+=("$obj")
done

for src in "$VENDOR_DIR"/external/runtime/*.cpp; do
  obj="$WORK_DIR/runtime_$(basename "${src%.cpp}").o"
  compile_cpp "$src" "$obj"
  if [[ "$(basename "$src")" == "runtime64_specific.cpp" ]]; then
    maybe_strip_main "$obj"
  fi
  OBJECTS+=("$obj")
done

skip_obj="$WORK_DIR/skip_bc.o"
compile_llvm "$VENDOR_DIR/external/skip.ll" "$skip_obj"
OBJECTS+=("$skip_obj")

LIB_PATH="$DEST_DIR/libskip_reactive.a"
rm -f "$LIB_PATH"
ar rcs "$LIB_PATH" "${OBJECTS[@]}"

echo "Built $LIB_PATH from vendored Skip runtime sources"
