# React Native platform module fixture

Permanent fixture for [`../../platforms`](../../platforms), promoted from the
original feasibility experiment. It has one `Button.resi`, Android/iOS
implementations with different abstract-type representations, and a shared
`App.res` that uses the logical `Button` module.

The acceptance tests exercise Rewatch's `"platforms": ["android", "ios"]`
configuration directly. `build.mjs` invokes the real Rewatch binary and contains
no platform build logic itself.

After building the checkout with `make lib`:

```sh
cd rewatch/tests/fixtures/react-native-platforms
npm test
```

This command needs no npm installation. It runs the core acceptance tests in
temporary projects. To select a different Rewatch executable, set
`REWATCH_EXECUTABLE`; `RESCRIPT_BSC_EXE` and `RESCRIPT_RUNTIME` override the local
compiler and runtime.

For the separate React Native/Metro integration check:

```sh
npm ci
npm run bundle
```

That command first runs Rewatch, builds Android and iOS bundles, and checks their source maps for
the correct variants. The fixture uses CommonJS with out-of-source `.js` output
under `lib/js/src`; `index.cjs` registers `PlatformExperiment` from that output.
The bundler requires one shared `App.js` output.

No simulator or native project scaffolding is needed: this tests the
compiler/build-system/bundler boundary. The core test stubs React and React Native
when executing generated modules. The bundle test uses the pinned real packages.
The shared interface and both platform implementations declare
`@react.component`, so JSX v4 derives their `props` type from `~title`. The
platform components render the native button through small local React and React
Native bindings, and the annotated `App` component renders the logical
`<Button>`.

The original low-level compiler probe remains available separately:

```sh
npm ci
npm run test:compiler
```

`probe-compiler.mjs` and `probe-compiler.test.mjs` demonstrate the existing compiler
primitives and reproduce the optimization/import pitfalls that require
conservative shared module metadata. They are **not the acceptance path**; the
Rewatch tests above define the supported behavior.
