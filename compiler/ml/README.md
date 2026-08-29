# Type checker

This directory contains the compiler's type representation, type checker, and
typed tree. It also contains several later frontend passes inherited from the
same compiler layer. This guide is an index to the type-checking code; the
module interfaces and local implementation comments define the detailed
contracts.

## Where to start

For a surface-language expression, declaration, or module feature, follow the
parsetree node through these modules:

- [`parsetree.ml`](parsetree.ml) defines the current untyped AST.
  `parsetree0.ml` is the frozen compatibility AST and must not be changed.
- [`typecore.ml`](typecore.ml) checks expressions and patterns.
- [`typetexp.ml`](typetexp.ml) translates source type expressions.
- [`typedecl.ml`](typedecl.ml) checks type declarations.
- [`typemod.ml`](typemod.ml) checks structures, signatures, and modules. Its
  `type_structure` entry point coordinates structure typing.
- [`typedtree.ml`](typedtree.ml) defines the result consumed by later compiler
  passes and tooling.

For a change to inference, unification, generalization, subtyping, or type
copying, start with the public contracts in [`ctype.mli`](ctype.mli) and
[`btype.mli`](btype.mli), then read the corresponding implementation around the
operation being changed. These modules operate on shared mutable type graphs;
local-looking mutations can affect aliases and speculative checks.

For module inclusion and signature compatibility, start with
[`includecore.mli`](includecore.mli), [`includemod.mli`](includemod.mli), and
[`mtype.mli`](mtype.mli).

## Main data and operations

[`types.mli`](types.mli)
: Internal type expressions and declarations. A `type_expr` is a mutable graph
  node, not an immutable syntax tree. Use the representation functions exposed
  by `Btype` before interpreting linkable state.

[`typedtree.mli`](typedtree.mli)
: Typed expressions, patterns, declarations, and modules. Nodes retain the
  environment in which they were checked.

[`env.mli`](env.mli)
: Typing environments, lookup, persistent signatures, and local constraints.

[`btype.mli`](btype.mli)
: Operations on the type representation: representatives, graph traversal,
  the mutation trail, snapshots, and scoped copying. Read its copy-session
  contract before calling `copy_type_desc` directly.

[`ctype.mli`](ctype.mli)
: Type-checking operations built on that representation: instantiation,
  generalization, unification, object-field constraints, enlargement, and
  subtyping.

[`subst.mli`](subst.mli)
: Substitution and copying across environments and persistence boundaries.
  `for_saving` has stronger independence requirements than an ordinary copy.

## Polymorphic value positions

A `Tpoly` node represents a type scheme, so the operation depends on whether
the surrounding syntax consumes or defines a value at that scheme:

- An elimination site instantiates the scheme for one use. Object-field reads
  do this in `object_field_use_type`.
- An introduction site must show that the expression is at least as general as
  the scheme. `type_let` for polymorphic annotations, `type_label_exp` for
  record fields, and `type_object_field_value` for object-field assignments all
  type the expression at a fixed instance and call `check_univars`.

The `fixed` argument of `Ctype.instance_poly` controls the copying of fixed
polymorphic-variant rows; it is not by itself an introduction/elimination
marker. The generality check is what distinguishes introduction. After a
successful check, the typed expression carries an ordinary instance rather
than the fixed checking instance.

Do not copy `type_label_exp`'s retry for expansive expressions into another
introduction site by default. That retry recovers completeness lost through
record-label type propagation and is specific to that typing path.

## Choosing the right level for documentation

- Put caller-observable requirements in a module interface. Examples include
  whether an operation mutates its input graph, requires a copy session, can
  leave deferred constraints, or must be paired with backtracking.
- Put representation invariants and algorithm ordering in the implementation
  that owns them. Examples include how copy-session marks are installed and
  restored, or why row openness is sampled before row tails are unified.
- Keep comments at call sites when a high-level algorithm combines mechanisms
  whose interaction is otherwise easy to miss. Coercion in `typecore.ml`, for
  example, has both an enlargement-and-unification path and a subtyping path.
- Put broad navigation and debugging advice here. Do not duplicate detailed
  invariants from source comments in this guide.

When a change introduces a new form of mutable state in a type node, document
at least its semantic states, representative operation, sharing and copying
rules, trail/backtracking behavior, and persistence behavior. Tests should
cover aliasing, instantiation independence, speculative failure, and saving
when those properties apply.

## Testing and inspection

From the repository root:

```sh
make                 # build the compiler and build system
make test            # build the library and run the complete test suite
make checkformat     # check formatting
```

Useful focused suites include `tests/ounit_tests/` for internal operations and
`tests/build_tests/super_errors/` for type errors. Add end-to-end cases under
`tests/tests/` when a type-system change also affects accepted programs or
generated JavaScript.

To inspect the compiler pipeline for a small source file:

```sh
./cli/bsc.js -dparsetree example.res
./cli/bsc.js -dtypedtree example.res
./cli/bsc.js -drawlambda example.res
```

Small source probes are useful evidence, but they do not by themselves test
graph sharing, backtracking, or persistence. Add a unit test when the property
cannot be observed reliably through source syntax.

## Background reading

The checker uses level-based generalization. Oleg Kiselyov's
[Efficient and Insightful Generalization](https://okmij.org/ftp/ML/generalization.html)
is useful background before changing generalization or instantiation. External
material explains the underlying techniques, but this repository's interfaces,
implementation comments, and tests define current ReScript behavior.
