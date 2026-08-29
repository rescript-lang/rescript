# Vendored Flow parser

This directory contains the OCaml Flow parser used by the ReScript compiler.

- Upstream repository: https://github.com/facebook/flow
- Flow parser version: `0.320.0`
- Parser and Sedlex source commit: `7c64d4b077bc6fc45c12cee3cfa7368fdb2186ce`
- Collection source commit: `9ea4062c0b7e037415c4413a7634c459ebd5c31b`
  from Flow parser 0.267.0
- Original source directories: `src/parser`, `src/third-party/sedlex`,
  `src/third-party/sedlex-ppx`, and `src/hack_forked/utils/collections`

The collection helpers retain the redistributable 0.267.0 sources. Their code
is identical in Flow 0.320.0, but that release replaces their license notices
with "Confidential and proprietary" notices that do not grant redistribution
rights. The Meta-authored collection modules are MIT licensed. `Flow_map` and
`Flow_set` are derived from the OCaml standard library and remain under LGPL
2.1 with the OCaml linking exception.

The Dune files were adapted to build these sources as private libraries inside
the ReScript repository. Sources used only by the upstream JavaScript and C API
targets are not included. One ambiguous Sedlex documentation comment was
converted to a regular comment and one unused loop index was renamed so the
vendored sources build with ReScript's warning settings. One `List.is_empty`
call uses an empty-list comparison to retain OCaml 5.0 compatibility. The
Sedlex PPX uses `Ast_helper.Exp.fun_` to generate single-argument functions
with newer ppxlib versions.

Only modules in the dependency closure of ReScript's expression and program
parser entry points are retained. Upstream ESTree translation, JSDoc parsing,
location translation, token translation, and unused collection helpers are
omitted. The core statement, declaration, type, JSX, pattern, comment, and AST
modules remain because the parser connects them transitively and `%raw`
validates both complete JavaScript programs and individual expressions.

Vendored sources are excluded from the repository-wide OCamlformat check so
that they remain comparable with their upstream versions.

The Flow parser and Meta-authored collection sources are licensed under the
MIT licence in `LICENSE` and their source headers. Vendored Sedlex and the
OCaml-derived collection sources retain the licence files in their respective
directories.
