#!/bin/bash

shopt -s extglob

echo Formatting OCaml code...
dune build @fmt --auto-promote

echo Formatting ReScript code...
files=$(find packages tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*" ! -name "generated_mocha_test.res" ! -path "tests/syntax_tests*" ! -path "tests/analysis_tests/tests*" ! -path "*/node_modules/*")
./cli/rescript.js format $files

echo Formatting JS code...
yarn format

echo Formatting Rust code...
cargo fmt --manifest-path rewatch/Cargo.toml

echo Done.
