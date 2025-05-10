#!/bin/bash
set -e
shopt -s extglob

(cd runtime && ../cli/rewatch.js clean)
(cd runtime && ../cli/rewatch.js build)

cp runtime/lib/es6/!(Pervasives_mini).js lib/es6
cp runtime/lib/js/!(Pervasives_mini).js lib/js
cp runtime/lib/bs/!(Pervasives_mini|Belt_internal*).cmi lib/ocaml/
cp runtime/lib/bs/!(Pervasives_mini).@(cmi|cmj|cmt|cmti) lib/ocaml/
cp runtime/!(Pervasives_mini).@(res|resi) lib/ocaml/