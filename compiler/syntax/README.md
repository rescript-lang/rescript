# Parser, printer, and JSX transformation

This directory owns ReScript source parsing, comment attachment, printing, and
the built-in JSX transformation. The parser is hand-written and produces the
parsetree consumed by `compiler/ml`.

## Code map

- [`src/res_scanner.ml`](src/res_scanner.ml) tokenizes source text.
- [`src/res_parser.ml`](src/res_parser.ml) and
  [`src/res_grammar.ml`](src/res_grammar.ml) implement parsing and recovery.
- [`src/res_comment.ml`](src/res_comment.ml) and
  [`src/res_comments_table.ml`](src/res_comments_table.ml) retain and attach
  comments for printing.
- [`src/res_printer.ml`](src/res_printer.ml),
  [`src/res_doc.ml`](src/res_doc.ml), and
  [`src/res_parens.ml`](src/res_parens.ml) implement formatting.
- [`src/jsx_ppx.ml`](src/jsx_ppx.ml) selects and applies the built-in JSX
  transformation; [`src/jsx_v4.ml`](src/jsx_v4.ml) implements the current
  transform.
- [`cli/res_cli.ml`](cli/res_cli.ml) provides the repository-only `res_parser`
  diagnostic tool. Production compiler code calls the syntax library APIs.

See [Formatter.md](Formatter.md) for formatter policy and [JSX.md](JSX.md) for
the current JSX transformation contract.

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
dune exec res_parser -- -print ml example.res
dune exec res_parser -- -print res -width 80 example.res
```

The CLI is for compiler development and tests; it is not a supported public
parser interface.

## Changing syntax

A syntax change can affect more than the grammar. Check each relevant layer:

1. scanner tokens and parser recovery;
2. the current parsetree in `compiler/ml/parsetree.ml`;
3. printing, parentheses, and comment attachment;
4. the v0 AST bridges in `compiler/ml/ast_mapper_from0.ml` and
   `compiler/ml/ast_mapper_to0.ml`;
5. type checking and every later compiler representation that carries the
   construct;
6. parser, round-trip, type-error, and end-to-end tests.

Do not modify `compiler/ml/parsetree0.ml`. It is the frozen input/output shape
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
