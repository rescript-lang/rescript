# Vendored Flow parser

This directory contains the OCaml Flow parser used by the ReScript compiler.

- Upstream fork: https://github.com/rescript-lang/flow
- Flow parser version: `0.267.0`
- Source commit: `9ea4062c0b7e037415c4413a7634c459ebd5c31b`
- Original source directories: `src/parser`, `src/third-party/sedlex`,
  `src/third-party/sedlex-ppx`, and `src/hack_forked/utils/collections`

The Dune files were adapted to build these sources as private libraries inside
the ReScript repository. Sources used only by the upstream JavaScript and C API
targets are not included. One ambiguous Sedlex documentation comment was
converted to a regular comment so the vendored sources build with ReScript's
warning settings.

Vendored sources are excluded from the repository-wide OCamlformat check so
that they remain comparable with their upstream versions.

The Flow sources are licensed under the MIT licence in `LICENSE`. Vendored
Sedlex and collections sources retain their own licence files.
