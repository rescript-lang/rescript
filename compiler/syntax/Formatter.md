# Formatter policy

The ReScript formatter is deliberately opinionated and has no formatting
configuration. The core team chooses formatting behavior to keep ReScript code
consistent across projects and to keep the printer maintainable.

Bug reports and proposals are welcome, but formatter changes are evaluated by
technical correctness, consistency with the language, implementation
complexity, and core-team consensus. Similar behavior in another formatter is
useful evidence, not by itself a reason to adopt a rule.

Some constructs preserve a source author's choice to make them multiline. For
example, the printer preserves meaningful line breaks in pipe chains and
records. These decisions describe current behavior rather than a general rule
that all source line breaks must be retained.

When changing the formatter:

- test both narrow and wide print widths;
- cover comments and the parentheses needed to preserve parsing;
- run `make test-syntax` and `make test-syntax-roundtrip`;
- inspect snapshot changes for unrelated reformatting;
- prefer a simple rule that behaves consistently across equivalent AST shapes.

The implementation map and diagnostic commands are in [README.md](README.md).
