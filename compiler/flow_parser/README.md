# Vendored Flow parser

This directory contains the OCaml Flow parser used by the ReScript compiler.

- Upstream repository: https://github.com/facebook/flow
- Flow parser version: `0.320.0`
- Source commit: `7c64d4b077bc6fc45c12cee3cfa7368fdb2186ce`
- Original source directories: `src/parser`, `src/third-party/sedlex`,
  `src/third-party/sedlex-ppx`, and `src/hack_forked/utils/collections`

The Dune files were adapted to build these sources as private libraries inside
the ReScript repository. Sources used only by the upstream JavaScript and C API
targets are not included. One ambiguous Sedlex documentation comment was
converted to a regular comment and one unused loop index was renamed so the
vendored sources build with ReScript's warning settings.

Vendored sources are excluded from the repository-wide OCamlformat check so
that they remain comparable with their upstream versions.

The Flow sources are licensed under the MIT licence in `LICENSE`. Vendored
Sedlex and collections sources retain their own licence files.
