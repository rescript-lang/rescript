# Vendored Flow parser

This directory contains the OCaml Flow parser used by the ReScript compiler.
It is a pruned fork: Flow's parser has moved to Rust, so there are no further
OCaml releases to sync with, and code ReScript does not use is removed like
dead code anywhere else in the repository (`make dead-code` covers it, except
the AST in `parser/flow_ast.ml`).

- Upstream repository: https://github.com/facebook/flow
- Flow parser version: `0.320.0`
- Parser and Sedlex source commit: `7c64d4b077bc6fc45c12cee3cfa7368fdb2186ce`
- Collection source commit: `9ea4062c0b7e037415c4413a7634c459ebd5c31b`
  from Flow parser 0.267.0
- Original source directories: `src/parser`, `src/third-party/sedlex`,
  `src/third-party/sedlex-ppx`, and `src/hack_forked/utils/collections`

The collection helpers `Flow_map` and `Flow_set` retain the 0.267.0 sources.
Their code is identical in Flow 0.320.0, but that release replaces their
license notices with "Confidential and proprietary" notices that do not grant
redistribution rights. They are derived from the OCaml standard library and
remain under LGPL 2.1 with the OCaml linking exception.

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

Beyond that, everything reanalyze reports as dead was removed: unused
functions, optional arguments, parse options and the token sink, debug
printers (`[@@deriving show]`), the file-key variants (ReScript parses only
strings), error variants that are never raised, and AST fields that the parser
fills in but nothing reads (template literal quasi values, tolerated-syntax
records, JSX text and fragment locations, comment `on_newline`, accessibility
comments). The AST mapper classes that moved trailing comments between nodes
(`Flow_ast_mapper` and its subclasses in `Comment_attachment`) are gone too:
ReScript never reads trailing comments, and creating the classes was most of
the compiler's startup work. They were also what read every AST field, so most
fields of `flow_ast.ml` are now only written; `make dead-code` does not check
that file.

Vendored sources are excluded from the repository-wide OCamlformat check so
that they keep Flow's formatting.

The Flow parser is licensed under the MIT licence in `LICENSE` and its source
headers. Vendored Sedlex and the OCaml-derived collection sources retain the
licence files in their respective directories.
