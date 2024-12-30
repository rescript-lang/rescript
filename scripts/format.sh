#!/bin/bash

shopt -s extglob

dune build @fmt --auto-promote

files=$(find runtime tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*" ! -path "tests/syntax_*" ! -path "tests/analysis_tests/tests*" ! -path "tests/gentype_tests/typescript-react-example/node_modules")
./cli/rescript format $files

npm run format
