# Parser and printer

This directory owns the parsetree, ReScript source parsing, comment
attachment, and printing, as three libraries of the `rescript-syntax` opam
package that depend on the standard library only and build with OCaml 4.14
or later: `ast/` (`rescript-syntax.ast`, module `Rescript_ast`), `printer/`
(`rescript-syntax.printer`, module `Rescript_printer`) and `parser/`
(`rescript-syntax.parser`, module `Rescript_parser`), which uses the printer
for some error messages. The compiler opens these modules with `-open` flags
in its dune files, so its code uses the inner modules (`Parsetree`,
`Res_printer`, ...) directly. The parser is hand-written and produces the parsetree consumed by
`compiler/typing`. The built-in JSX transformation, which rewrites that
parsetree, lives in [`compiler/frontend`](../compiler/frontend/JSX.md).

## Code map

- [`parser/res_scanner.ml`](parser/res_scanner.ml) tokenizes source text.
- [`parser/res_core.ml`](parser/res_core.ml) implements the grammar: parsing and
  error recovery.
- [`parser/res_parser.ml`](parser/res_parser.ml) holds the parser state: the token
  cursor, lookahead, breadcrumbs and diagnostics.
- [`parser/res_grammar.ml`](parser/res_grammar.ml) defines the grammar contexts
  (breadcrumbs) and the token classes that recovery uses to decide where a
  construct starts or ends.
- [`printer/res_comment.ml`](printer/res_comment.ml) and
  [`printer/res_comments_table.ml`](printer/res_comments_table.ml) retain and attach
  comments for printing.
- [`printer/res_printer.ml`](printer/res_printer.ml),
  [`printer/res_doc.ml`](printer/res_doc.ml), and
  [`printer/res_parens.ml`](printer/res_parens.ml) implement formatting.
- [`compiler/syntax_cli/res_cli.ml`](../compiler/syntax_cli/res_cli.ml)
  provides the repository-only `res_parser` diagnostic tool, with its AST,
  token and OCaml-syntax debug printers in `compiler/syntax_cli/`. Production compiler code calls the syntax library APIs.

See [Formatter.md](Formatter.md) for formatter policy.

[ParserCursor.md](ParserCursor.md) states the parser's token-cursor invariants (peek versus next, lookahead and try_parse rollback, pending comments and diagnostics); read it before changing `Res_parser` consumption or recovery.

## Building and testing

Run commands from the repository root:

```sh
make                         # build the compiler and build system
make test-syntax             # parser and printer tests
make test-syntax-roundtrip   # parse/print round-trip tests
make checkformat             # check repository formatting
```

Use the repository diagnostic CLI to inspect one file:

```sh
dune exec res_parser -- example.res
dune exec res_parser -- -print tokens example.res
dune exec res_parser -- -print ast -recover example.res
dune exec res_parser -- -print comments example.res
dune exec res_parser -- -print doc example.res
dune exec res_parser -- -print ml example.res
dune exec res_parser -- -print res -width 80 example.res
```

The CLI is for compiler development and tests; it is not a supported public
parser interface.

## Changing syntax

A syntax change can affect more than the grammar. Check each relevant layer:

1. scanner tokens and parser recovery;
2. the current parsetree in `syntax/ast/parsetree.ml`;
3. printing, parentheses, and comment attachment;
4. the v0 AST bridges in `compiler/ast0/ast_mapper_from0.ml` and
   `compiler/ast0/ast_mapper_to0.ml`;
5. type checking and every later compiler representation that carries the
   construct;
6. parser, round-trip, type-error, and end-to-end tests.

Do not modify `compiler/ast0/parsetree0.ml`. It is the frozen input/output shape
for existing PPX integrations. When the current parsetree changes, define an
explicit compatibility mapping in both directions; do not use a wildcard to
discard a new construct.

Parser tests should cover valid input, recovery from invalid input, printing,
and comment placement where applicable. Run the round-trip suite whenever a
change affects parsing or printing, even when the intended AST is unchanged.

## Documentation placement

Public language behavior belongs on the ReScript website. This directory keeps
implementation-facing documentation needed to change the parser, printer, or
built-in transformations. Put API contracts in `.mli` files and local parsing
or printing invariants beside their implementation.
