#!/bin/bash

shopt -s extglob

dune build @fmt --auto-promote

files=$(find runtime tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*" ! -name "generated_mocha_test.res" ! -path "tests/syntax_*" ! -path "tests/analysis_tests/tests*" ! -path "*/node_modules/*")
./cli/rescript format $files

npm run format
