#!/bin/bash

shopt -s extglob

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

case "$(uname -s)" in
  Darwin|Linux)
    echo "Checking OCaml code formatting..."
    if opam exec -- dune build @fmt; then
      printf "${successGreen}✅ OCaml code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ OCaml code formatting issues found.${reset}\n"
      exit 1
    fi

    echo "Checking ReScript code formatting..."
    files=$(find packages tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*" ! -name "generated_mocha_test.res" ! -path "tests/syntax_tests*" ! -path "tests/analysis_tests/tests*" ! -path "*/node_modules/*")
    if ./cli/rescript.js format --check $files; then
      printf "${successGreen}✅ ReScript code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ ReScript code formatting issues found. Run 'make format' to fix.${reset}\n"
      exit 1
    fi
    ;;
  *)
    # Does not work on Windows
    echo "Code formatting checks skipped for this platform."
esac

echo "Checking JS code formatting..."
if yarn check; then
  printf "${successGreen}✅ JS code formatting ok.${reset}\n"
else
  printf "${warningYellow}⚠️ JS code formatting issues found.${reset}\n"
  exit 1
fi

echo "Checking Rust code formatting..."
if cargo fmt --check --manifest-path rewatch/Cargo.toml; then
  printf "${successGreen}✅ Rust code formatting ok.${reset}\n"
else
  printf "${warningYellow}⚠️ Rust code formatting issues found.${reset}\n"
  exit 1
fi
