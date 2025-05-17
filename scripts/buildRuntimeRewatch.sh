#!/bin/bash
set -e
shopt -s extglob

(cd runtime && ../cli/rewatch.js clean && ../cli/rewatch.js build)

rm -f lib/es6/*.js lib/js/*.js lib/ocaml/*
mkdir -p lib/es6 lib/js lib/ocaml

cp runtime/lib/es6/*.js lib/es6
cp runtime/lib/js/*.js lib/js
cp runtime/lib/bs/*.@(cmi|cmj|cmt|cmti) lib/ocaml/
cp runtime/*.@(res|resi) lib/ocaml/
