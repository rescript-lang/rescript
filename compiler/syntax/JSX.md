# Built-in JSX transformation

This document describes the current compiler-facing JSX transformation. For
language usage and project configuration, use the
[JSX configuration manual](https://rescript-lang.org/docs/manual/build-configuration/#jsx)
and the [ReScript React documentation](https://rescript-lang.org/docs/react/beyond-jsx/).

## Entry points and configuration

[`jsx_ppx.ml`](src/jsx_ppx.ml) applies the built-in transformation to
implementations and signatures. It receives the project JSX version and module
from the compiler driver. A `@jsxConfig` structure or signature attribute can
override `version` and `module_` for the following items in that scope; nested
structures and signatures save and restore the enclosing configuration.

The current transformation is version 4, implemented by
[`jsx_v4.ml`](src/jsx_v4.ml). A non-React JSX module selects the same transform
and changes the module paths emitted by it. Configuration state is local to a
mapper invocation and includes the current nested-module path and whether the
scope already defines a component.

## Component definitions

`@react.component` and `@jsx.component` mark a component definition. The
transform validates its labelled parameters and generates the props type and
wrapper required by the JSX runtime. Only one component definition is allowed
in a module; additional components must be placed in nested modules or
separate files.

The same transformation is applied to signatures so the implementation and
interface expose compatible component types. External components and the
legacy `componentWithProps` form have separate validation paths; tests for
these forms live under `tests/build_tests/react_ppx/`.

## JSX expressions

The parser represents JSX explicitly in the parsetree. The transformation
rewrites those nodes as calls to the configured JSX module:

- an uppercase tag normally denotes the module's `make` component;
- a qualified tag whose final component is lowercase denotes that value
  directly, which supports external components;
- a lowercase tag is emitted through the configured host-element module;
- fragments use the configured `jsxFragment` value;
- one child becomes a `children` prop and multiple children become an array;
- keyed elements select the keyed runtime entry point;
- at most one props spread is accepted, and it must precede explicit props.

The precise runtime entry points differ between React host elements and a
generic JSX module. Read `mk_react_jsx`, `append_children_prop`, and
`mk_uppercase_tag_name_expr` in `jsx_v4.ml` together when changing expression
lowering.

## Change checklist

A change to this transform normally needs all of the following:

- implementation and signature handling kept in agreement;
- React and generic-module cases checked separately;
- parser/printing coverage if the surface JSX shape changes;
- transform snapshots under `tests/syntax_tests/`;
- component type and diagnostic coverage under `tests/build_tests/`;
- analysis and GenType coverage when the generated component shape changes.

Use `dune exec res_parser -- -jsx-version 4 -print ml example.res` to inspect
the transformed parsetree during development. The CLI flag is a diagnostic
interface, not a supported project configuration mechanism.
