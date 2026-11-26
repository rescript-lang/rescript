# ReScript React Todo

From the ReScript repo root:

| Command | What it does |
| --- | --- |
| `yarn todo:build` | Builds the ReScript app, checks TypeScript, and bundles it into `dist/`. |
| `yarn todo:check` | Tests the app, checks the generated snapshots and dependency boundary, and runs TScert and Biome. |
| `yarn todo:dev` | Starts the local dev server. |

### Install the checker

Use Node 26.13.0 for the pinned TScert research preview. The preview supports
Apple Silicon macOS and Linux x64. Linux also needs Bubblewrap and
unprivileged user namespaces.

```sh
yarn install
yarn todo:check
```

The check runs one TScert batch for the app and one for its controls. For
checker development, set `TSCERT_DIR` to a local `single-pass` checkout or
set `TSCERT_BIN` to an installed CLI that supports `--batch`.

## Verification boundary

```text
src/trusted/  mounts React, owns useReducer, reads DOM input values
    |
    | state, dispatch(action), input component with onValue(string)
    v
src/app/      ReScript model, reducer, and view components
              emitted TypeScript bundled by Vite and checked by TScert
```

The verification project supplies the [React type model](contracts/jsx-runtime.d.ts)
to TScert. The reducer's `@tscert.noMutation` attribute emits
`Modifies<{}, state>` on `reduce` itself. TScert checks the claim against
the emitted body and the functions it calls. The generated file references
`@cristianoc/tscert/effects` from the installed TScert package so TypeScript
reads the annotated return type as `state`.

The [app](src/app/) receives state and produces typed actions. Its input
component receives a string value and a string callback. The app imports
only other app modules and the JSX runtime. The build test enforces that
dependency rule on the emitted files.

The [trusted layer](src/trusted/) mounts React, calls `useReducer` with the
verified reducer, and passes the input's string value during event delivery.
TypeScript checks this layer. TScert checks the app and assumes the supplied
props and the [JSX runtime contract](contracts/jsx-runtime.d.ts). The verdict
covers the reducer's type and empty write footprint on normal return, along
with the component bodies and handlers. React's rendering, event delivery,
and update ordering are outside that verdict. Biome checks hook placement.

The check runs the negative cases in [`controls/`](controls/).
