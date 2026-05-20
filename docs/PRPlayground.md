# Per-PR Developer Playground Plan

## Goal

Build and publish a lightweight developer playground for every pull request, using the existing browser compiler bundle as the source of truth and a small Xote/Vite frontend as the UI shell.

The playground should serve two workflows:

- Local compiler development: run the exact branch compiler in a browser at localhost.
- Pull request review: open a stable `github.io` URL for a PR and inspect compiler output without checking out the branch.

This is separate from the end-user playground on `rescript-lang.org/try`. The developer playground can expose compiler internals, experimental settings, unstable tabs, and per-PR builds.

## Starting Point

The repo already builds a browser compiler bundle:

- `make playground` builds `packages/playground/compiler.js` through the `browser` dune profile.
- `packages/playground/scripts/generate_cmijs.mjs` emits side-loadable `cmij.js` package metadata.
- `packages/playground/serve-bundle.mjs` can serve a local bundle through `/playground-bundles/<version>/...`.
- CI already builds and tests the playground bundle on the ARM Linux matrix entry, and uploads release-tag bundles to Cloudflare R2.

The external prototype at `https://github.com/mununki/rescript-playground-xote/tree/main` should be the implementation baseline, not just inspiration:

- Xote + Vite frontend with a small footprint.
- Versioned bundle loading from `public/playground-bundles/<version>/`.
- Tabs for parsetree, typedtree, lambda, lam, JavaScript, and settings.
- URL-encoded source/settings for shareable examples.

Keep its layout, interaction model, and visual structure unless there is a concrete compiler-developer requirement that it cannot satisfy. The goal is to bring that app into this repo and adapt the bundle/catalog plumbing, not to redesign the playground.

The current compiler bundle API is narrower than that prototype assumes. Upstream `compiler/jsoo/jsoo_playground_main.ml` currently exposes compile output, warnings, type hints, formatting, and settings such as module system, warn flags, open modules, experimental features, and JSX preserve mode. A developer playground needs a deliberate expansion of that API rather than adding UI-only placeholders.

## Proposed Repository Shape

Add a new workspace package:

```txt
packages/dev-playground/
  package.json
  rescript.json
  vite.config.js
  index.html
  src/
    Main.res
    CompilerApi.res
    CompilerRuntime.js
    UrlState.js
    styles.css
  scripts/
    stage-local-bundle.mjs
    build-catalog.mjs
    validate-dist.mjs
```

Keep `packages/playground/` as the compiler-bundle package. It should remain responsible for producing `compiler.js` and cmij package files. The new `packages/dev-playground/` package should be a frontend shell that consumes those assets.

This split keeps the end-user bundle pipeline reusable and prevents UI work from being coupled to compiler bundle generation.

Seed `packages/dev-playground/` from the shared Xote implementation and keep the initial patch mechanical:

- preserve the two-column source/output layout,
- preserve the tab strip and settings panel structure,
- preserve URL state handling and share links,
- preserve the existing lightweight editor approach,
- replace only the hardcoded vendored bundle list with local/master/PR catalog loading,
- add new output tabs through the artifact registry rather than changing the main layout.

The frontend source should still be ReScript. The deployment dependency should be deliberate:

- The compiler payload under review must always be the `compiler.js` and cmij bundle built from the same branch/SHA as the pull request.
- The static UI shell should be built with `rescript@12.3.0`, the latest stable compiler, so a PR compiler regression does not prevent publishing a playground that can display that regression.
- Local development can use the workspace compiler for convenience, but the UI should avoid depending on brand-new language features unless the goal is specifically to dogfood them.

The `packages/dev-playground/package.json` dependencies should therefore pin:

```json
{
  "dependencies": {
    "@rescript/runtime": "12.3.0",
    "rescript": "12.3.0",
    "vite": "...",
    "xote": "..."
  }
}
```

## Local Developer Workflow

Add Makefile targets:

```make
dev-playground: playground
	yarn workspace dev-playground stage-local-bundle
	yarn workspace dev-playground dev

dev-playground-build: playground
	yarn workspace dev-playground stage-local-bundle
	yarn workspace dev-playground build
```

Expected commands:

```sh
make dev-playground
make dev-playground-build
```

The `stage-local-bundle` script should copy or symlink the local bundle into:

```txt
packages/dev-playground/public/playground-bundles/local/
  compiler.js
  compiler-builtins/cmij.js
  compiler-builtins/stdlib/*.js
  @rescript/react/cmij.js
```

The local UI should default to `local`. Local development must not require network access or a released bundle catalog.

## Compiler Bundle API

Introduce an API version bump in `compiler/jsoo/jsoo_playground_main.ml`.
The initial v7 API should stay additive:

- Keep `rescript.compile(source)` unchanged for the end-user playground and existing CDN bundles.
- Add `rescript.compileWithDebug(source, outputs)` for developer tooling.
- Return the same success/error shape as `compile`, plus only the requested debug output string fields.

This keeps `rescript-lang.org/try` compatible while allowing the developer playground to feature-detect `api_version >= 7`.

Suggested result shape:

```rescript
type diagnostic = {
  fullMsg: string,
  shortMsg: string,
  row: option<int>,
  column: option<int>,
}

type typeHint

type artifactContent =
  | Text(string)
  | Json(string)
  | Files(array<(string, string)>)

type artifact = {
  title: string,
  content: artifactContent,
  mime: option<string>,
}

type compileResult =
  | Success({
      jsCode: string,
      warnings: array<diagnostic>,
      typeHints: array<typeHint>,
      artifacts: Dict.t<artifact>,
    })
  | Error({
      kind: string,
      warnings: array<diagnostic>,
      errors: array<diagnostic>,
      message: string,
    })
}
```

The raw `compiler.js` boundary can stay a plain JavaScript object for compatibility. The frontend should normalize that object into ReScript types close to the shape above before rendering tabs. The artifact map can be a frontend adapter over the current v7 fields first, and become a raw compiler result field later if that proves useful.

Initial artifact keys:

- `parsetree`
- `typedtree`
- `lambda`
- `lam`
- `js`
- `warnings`
- `type_hints`

Planned artifact keys:

- `source_map`
- `gentype`
- `cmt_summary`
- `compiler_args`
- `build_graph`
- `analysis`

Avoid adding one top-level field per future output. A map of named artifacts lets compiler developers add tabs without forcing frontend rewrites.

## Supporting `rescript.json`

The first version should support the settings that map cleanly to one-file browser compilation:

- `package-specs.module`: `esmodule` or `commonjs`
- `suffix`
- `namespace`
- `warnings`
- `jsx`
- `uncurried`
- `compiler-flags`, for a curated safe subset
- `open_modules`
- `experimental_features`
- `gentypeconfig`, once GenType output is wired

The UI should expose a `rescript.json` editor next to the source editor. Internally, parse the JSON into a normalized playground config and show unsupported fields as warnings, not hard failures.

Fields that require a real project graph should be explicitly marked as staged support:

- `sources`
- `dependencies`
- `dev-dependencies`
- `ppx-flags`
- generators/resources
- `reanalyze`

For these, the playground should initially support a small virtual project model:

```txt
/rescript.json
/src/Playground.res
/src/Playground.resi
/src/OtherModule.res
```

The virtual project model is the prerequisite for reliable module dependencies, interface files, source maps, GenType, and analysis.

## Output Tabs

Use a dynamic tab registry driven by the compiler result artifacts:

- Always show source, JavaScript, problems, and settings.
- Show compiler internals only when present in `result.artifacts`.
- Preserve tab state through URL parameters.
- Allow each tab to choose a renderer: plain text, JSON tree, generated files, diagnostics list, or source-map explorer.

This keeps the UI useful when switching between the local bundle, the latest `master` bundle, and PR bundles with different API versions.

## Source Maps

Source maps should be a second-phase compiler artifact:

1. Extend the browser compiler API to optionally emit source map text alongside `js_code`.
2. Add a `source_map` artifact containing raw JSON.
3. Add a frontend source-map tab with:
   - raw JSON view,
   - generated-to-source mapping table,
   - click sync between ReScript source, generated JavaScript, and mapping entries.

The source-map tab should be hidden unless the compiler returns the artifact.

## GenType

GenType support should be built around generated files, not a single text field.

Proposed artifact:

```rescript
{
  title: "GenType",
  content: Files([
    ("Playground.gen.tsx", "..."),
    ("Playground.gen.ts", "..."),
  ]),
  mime: None,
}
```

Implementation path:

1. Support `gentypeconfig` parsing in the playground config.
2. Teach the browser compiler flow to run the GenType path over in-memory typed output, or write into the js_of_ocaml virtual filesystem if that is less invasive.
3. Display generated files as tabs inside the GenType panel.
4. Add tests that compile a file with `@genType` and assert generated TypeScript output exists.

## Analysis Binary and Completion

Treat analysis as optional and behind a separate milestone. The current analysis binary reads `.cmt` and `.cmti` files, so browser support likely requires either a virtual filesystem integration or a dedicated js_of_ocaml analysis bundle.

Recommended approach:

1. First expose enough compiler artifacts to generate `.cmt`-equivalent data in memory or in the jsoo filesystem.
2. Build a separate `analysis.js` bundle only when requested by the UI.
3. Lazy-load it from the playground when the user enables completions.
4. Keep the main playground bundle usable without analysis loaded.

Success criteria:

- Main playground bundle size does not materially grow when analysis is disabled.
- Completion works for the virtual project model.
- Analysis failures are isolated to the completion panel and never break compile output.

If the analysis bundle is too large, keep completion local-only through a Node helper process instead of shipping it to Pages.

## GitHub Pages Deployment

Use GitHub Pages for the UI shell and PR bundle catalog:

```txt
https://rescript-lang.github.io/rescript/dev-playground/
https://rescript-lang.github.io/rescript/dev-playground/pr/<pr-number>/<sha>/
https://rescript-lang.github.io/rescript/dev-playground/pr/<pr-number>/latest/
```

Recommended Pages layout:

```txt
dev-playground/
  index.html
  assets/...
  catalog.json
  playground-bundles/
    master/
  pr/
    1234/
      latest.json
      <sha>/
        catalog-entry.json
        playground-bundles/local/...
```

The initial deployed catalog should contain only:

- the latest `master` compiler bundle,
- PR builds generated after this feature lands.

Do not backfill released compiler bundles at the start. The shell should read `catalog.json` and `pr/<number>/latest.json` to populate a build selector from those entries only.

For forked PRs, avoid running privileged deployment on untrusted code. Use a two-step flow:

1. Build and upload a normal Actions artifact in the pull request workflow.
2. A trusted `workflow_run` job, scoped to artifacts only, publishes the static artifact to Pages.

## Deployment Structure

This should be implemented with GitHub Actions. There is no server component: Actions builds static files, GitHub Pages serves them.

For the first PR, keep GitHub Pages to the UI shell and the latest `master` compiler bundle. That is small and operationally simple.

Do not plan on storing unbounded PR bundles in GitHub Pages. GitHub Pages has a published site size limit, deployment timeout, and soft bandwidth limit, so it is a poor fit for a growing archive of compiler bundles. PR bundle publishing should use one of these policies:

- Short-term Pages storage: keep only the latest bundle per open PR, delete it when the PR closes, and enforce a hard cap on total PR bundles.
- Preferred longer-term storage: keep the UI and `master` bundle on GitHub Pages, but upload PR compiler bundles to Cloudflare R2/CDN under a `dev-playground/pr/<pr>/<sha>/` prefix, then write those bundle URLs into `catalog.json`.
- In either storage model, PR bundles must be cleaned up when the PR is merged or closed.

Use a generated `gh-pages` branch as the persistent state store for deployed playground files if PR bundles are stored on Pages. A plain Pages artifact deployment is a full-site snapshot, so it is awkward for appending PR bundle folders over time unless the workflow can reconstruct all previous PR builds. The `gh-pages` branch gives publish jobs a simple source of truth:

1. Check out `gh-pages` into a staging directory.
2. Copy new generated files into that staging directory.
3. Update `catalog.json` and any `latest.json` files.
4. Commit and push `gh-pages`.
5. Upload the staged directory as a Pages artifact and deploy it.

Recommended setup:

- Configure the repository's Pages source to GitHub Actions.
- Keep the generated site out of `master`.
- Use a serialized deployment concurrency group, for example `dev-playground-pages`, so two PR publish jobs cannot update `catalog.json` at the same time.
- Use real copied files for deployment artifacts. Local symlinks are fine for development, but Pages artifacts must not contain symbolic or hard links.

Suggested workflow split:

```txt
.github/workflows/dev-playground-master.yml
.github/workflows/dev-playground-pr-build.yml
.github/workflows/dev-playground-pr-publish.yml
.github/workflows/dev-playground-pr-cleanup.yml
```

`dev-playground-master.yml`:

1. Runs on push to `master`.
2. Builds the UI shell with `rescript@12.3.0`.
3. Builds and tests the browser compiler bundle from `master`.
4. Checks out or creates `gh-pages`.
5. Replaces:
   - `dev-playground/index.html`
   - `dev-playground/assets/`
   - `dev-playground/playground-bundles/master/`
6. Updates the `master` entry in `dev-playground/catalog.json`.
7. Deploys the full staged `gh-pages` directory to Pages.

`dev-playground-pr-build.yml`:

1. Runs on pull requests.
2. Builds and tests only the PR compiler bundle and cmij files.
3. Uploads a normal Actions artifact containing:
   - `compiler.js`
   - staged `packages/`
   - `catalog-entry.json`
4. Does not deploy and does not receive Pages write permissions.

`dev-playground-pr-publish.yml`:

1. Runs from a trusted context after the PR build workflow completes.
2. Downloads the PR artifact.
3. Does not execute code from the artifact.
4. Copies files into Pages or uploads them to R2/CDN:

   ```txt
   dev-playground/pr/<pr-number>/<sha>/playground-bundles/local/
   dev-playground/pr/<pr-number>/latest.json
   ```

5. Updates `dev-playground/catalog.json`.
6. Commits and pushes `gh-pages`.
7. Deploys the full staged `gh-pages` directory to Pages.

For forked PRs, either require a maintainer label before publishing or initially skip automatic publishing. A PR compiler bundle is executable JavaScript under the `github.io` origin, so publishing arbitrary fork output should be an explicit project policy.

`dev-playground-pr-cleanup.yml`:

1. Runs on `pull_request_target` for `closed` PRs.
2. Removes `dev-playground/pr/<pr-number>/` from `gh-pages` if PR bundles were stored on Pages.
3. Deletes the `dev-playground/pr/<pr-number>/` prefix from R2/CDN if PR bundles were stored there.
4. Removes the PR entry from `dev-playground/catalog.json`.
5. Deploys the updated Pages site.

Run cleanup for both merged and unmerged closes. A merged PR is represented by `pull_request.closed` with `pull_request.merged == true`; cleanup should not depend on that flag except for logging.

## CI Workflow

Keep the developer playground workflows separate from the existing release upload path.

Initial first PR requirements:

- Add local build verification for `make dev-playground-build`.
- Add master-only deployment if Pages is configured.
- Keep PR bundle publishing out of the first PR.

Second PR requirements:

- Add the PR bundle artifact workflow.
- Add the trusted PR publish workflow.
- Add PR comments with the deployed URL.
- Add cleanup for merged/closed PRs and retention rules for stale bundle folders.

Release job:

Keep the existing Cloudflare R2 release upload for the public playground. Do not include release bundle mirroring in the initial developer playground workflow.

## Relationship to `pkg.pr.new`

`pkg.pr.new` should stay in the plan, but it should not be treated as the browser playground deployment mechanism.

It publishes installable npm packages for Node-based integration testing. The browser playground needs the js_of_ocaml `compiler.js` bundle and cmij assets, which are not the same artifact.

Good integration points:

- Show the matching `pkg.pr.new` install URL in the PR playground header.
- Add it to `catalog-entry.json` so reviewers can copy the package install command.
- Use the same commit SHA for both the npm package link and the browser playground build.

Do not make the browser UI download compiler code from `pkg.pr.new` unless a dedicated browser-bundle package is introduced later.

## Testing

Compiler bundle tests:

- Existing `yarn workspace playground test`.
- Add tests for every artifact key that the compiler claims to expose.
- Snapshot minimal parsetree, typedtree, lambda, lam, source-map, and GenType outputs.

Frontend tests:

- Build test for `packages/dev-playground`.
- Smoke test that `dist/index.html` loads a staged local bundle.
- Browser smoke test for:
  - initial compile,
  - switching tabs,
  - editing `rescript.json`,
  - showing unsupported config warnings,
  - loading a PR catalog entry.

CI deployment tests:

- Validate `catalog.json` schema.
- Validate that each catalog entry has `compiler.js` and `compiler-builtins/cmij.js`.
- For PR artifacts, validate `latest.json` points at an existing SHA directory.

## Rollout

Recommended implementation order:

1. Land the local playground path first.
   - Seed `packages/dev-playground/` from the shared Xote implementation.
   - Pin the UI shell to `rescript@12.3.0`.
   - Add `stage-local-bundle`, `make dev-playground`, and `make dev-playground-build`.
   - Make `make dev-playground` depend on `make playground` so the current checkout's compiler bundle is built before the UI starts.
   - Add the additive v7 `compileWithDebug` browser compiler API.
   - Verify the UI against the current `master` compiler bundle through `playground-bundles/local`.
2. In the same first PR, add a master-only deployment if the GitHub Pages target is already available.
   - Build the UI shell with `rescript@12.3.0`.
   - Build the `master` compiler bundle.
   - Publish only `dev-playground/`, `catalog.json`, and `playground-bundles/master/`.
   - Do not publish PR artifacts yet.
   - This gives reviewers a deployed playground immediately after the first PR merges, with only the latest `master` compiler available.
3. Add PR build publishing in a second PR.
   - Add the untrusted PR artifact build.
   - Add the trusted publish job.
   - Add PR comments with the deployed URL.
   - Add cleanup/retention policy for old PR builds.
4. Generalize the compiler/frontend result adapter if future outputs need richer artifact metadata.
5. Add `rescript.json` editor support for the safe one-file settings.
6. Add source-map output.
7. Add GenType generated-file output.
8. Prototype lazy-loaded analysis/completion and decide whether it ships to Pages or stays local-only.

If Pages setup or repository permissions are uncertain, keep the first PR to local-only plus CI build verification, then make master deployment the second PR and PR build publishing the third. If Pages is straightforward, local support plus master-only deployment is a reasonable first PR because it avoids the untrusted-code complexity of PR artifact publishing.

## Open Questions

- Should the developer playground live at `rescript-lang.github.io/rescript/dev-playground/` or in a separate `rescript-lang.github.io/dev-playground/` project site?
- How many PR bundle versions should be retained, and who cleans them up after PR close?
- Should PR playground deployment be allowed for forks automatically, or only after maintainer approval?
- Should virtual multi-file projects be URL-shareable, or should large examples require GitHub gist/import support?
- Is the current `compiler/jsoo` build profile the right place for all debug artifacts, or should debug-heavy output be gated behind a separate bundle flavor?
