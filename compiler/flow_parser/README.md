# Vendored Flow parser

This directory contains the OCaml Flow parser used by the ReScript compiler.

- Upstream repository: https://github.com/facebook/flow
- Flow parser version: `0.320.0`
- Parser and Sedlex source commit: `7c64d4b077bc6fc45c12cee3cfa7368fdb2186ce`
- Collection source commit: `9ea4062c0b7e037415c4413a7634c459ebd5c31b`
  from Flow parser 0.267.0
- Original source directories: `src/parser`, `src/third-party/sedlex`,
  `src/third-party/sedlex-ppx`, and `src/hack_forked/utils/collections`

The collection helpers retain the MIT-licensed 0.267.0 sources. Their code is
identical in Flow 0.320.0, but that release replaces their MIT notices with
"Confidential and proprietary" notices that do not grant redistribution
rights.

The Dune files were adapted to build these sources as private libraries inside
the ReScript repository. Sources used only by the upstream JavaScript and C API
targets are not included. One ambiguous Sedlex documentation comment was
converted to a regular comment and one unused loop index was renamed so the
vendored sources build with ReScript's warning settings. One `List.is_empty`
call uses an empty-list comparison to retain OCaml 5.0 compatibility.

Vendored sources are excluded from the repository-wide OCamlformat check so
that they remain comparable with their upstream versions.

The Flow parser and retained collection sources are licensed under the MIT
licence in `LICENSE` and their source headers. Vendored Sedlex and collection
third-party sources retain the licence files in their respective directories.
