# Features

Features let a package declare optional parts of its source tree that can be included or excluded at build time. Use them to ship a library with optional backends, experimental modules, or platform-specific code without paying the compile cost when a consumer doesn't need them.

Features are a `rewatch` extension and are not part of the legacy `bsb` build-configuration spec.

## Tagging a source directory

Add a `feature` property to any entry in `sources`. The directory is included in the build only when the feature is active for that package.

```json
{
  "name": "@example/lib",
  "sources": [
    { "dir": "src" },
    { "dir": "src-native", "feature": "native" },
    { "dir": "src-experimental", "feature": "experimental" }
  ]
}
```

Untagged source directories (no `feature`) are always compiled. A tagged source's `feature` cascades down into nested `subdirs`: a child that doesn't declare its own `feature` inherits the parent's.

## Declaring feature relationships

A top-level `features` map declares names and optional implications. Listing a feature here is only required when you want one feature to imply another; leaf features can stay undeclared and still work as source-dir tags.

```json
{
  "features": {
    "full": ["native", "experimental"]
  }
}
```

With the above, requesting `full` transitively enables `native` and `experimental`. Cycles (e.g. `a -> b -> a`) are rejected at build time with a clear error.

## Selecting features on the command line

When you run `rewatch build` or `rewatch watch`, pass `--features` to restrict compilation to a specific set. Without the flag, every feature is active and the whole source tree compiles:

```
rewatch build                            # all features active (default)
rewatch build --features native          # only untagged + native
rewatch build --features native,full     # multiple features; also expands `full`
```

The CLI flag applies only to the **current package** — the one you're building from. It does not flow down to dependencies; each dependency's active feature set comes from its consumer declarations (see below).

Passing an empty value (`--features ""` or `--features ,`) is rejected. Omit the flag to mean "all features".

## Restricting a dependency's features

When consuming another ReScript package that uses features, switch the entry in `dependencies` or `dev-dependencies` from the shorthand string to an object form and list which features you want:

```json
{
  "dependencies": [
    "@plain/dep",
    { "name": "@example/lib", "features": ["native"] }
  ]
}
```

Rules:

- **Shorthand (`"@plain/dep"`)** — the consumer wants every feature of that dependency. This is the existing behavior; nothing changes for configs that don't opt into features.
- **Object with `features`** — the consumer restricts the dependency to the listed features (and whatever they transitively imply through the dependency's own `features` map). An explicit empty list (`"features": []`) means "only untagged source dirs, no feature-gated code".
- **Object without `features`** — equivalent to the shorthand. All features active.

When the same dependency is referenced by multiple consumers with different feature sets, the union of requests wins. If any consumer asks for all features, the dependency builds with all of its features. Features are always additive — enabling more features never removes modules, so the union is always safe.

## Interaction with other flags

- **`type: "dev"` and `--prod`** are orthogonal to features. A source directory may declare both `type: "dev"` and a `feature`; it will only build when both filters pass (not in `--prod`, and the feature is active).
- **`rewatch clean`** ignores `--features` and always cleans the full set of build artifacts across every feature-gated directory. This keeps `clean` predictable regardless of which features happen to be active.

## How incremental builds handle feature changes

Toggling a feature off between builds removes its source files from the build's view. The next `rewatch build` sees the shrunken file set and cleans up the corresponding artifacts (`.mjs`, `.cmj`, etc.) through the same diff mechanism that handles deleted source files.

For `rewatch watch`, a change to `features` in `rescript.json` triggers a full rebuild that recomputes the active set and re-registers watches on the active source directories. The CLI `--features` flag is evaluated once at watcher start; to change it you must restart the watcher.

## Validating feature names

- Unknown feature names in CLI input or source tags are accepted as leaf features (they simply match nothing unless a source directory is tagged with that exact name).
- Cycles in the top-level `features` map are a hard error that names the cycle participants.

## Example

```json
{
  "name": "@example/lib",
  "sources": [
    { "dir": "src" },
    { "dir": "src-native", "feature": "native" },
    { "dir": "src-web", "feature": "web" },
    { "dir": "src-experimental", "feature": "experimental" }
  ],
  "features": {
    "all-backends": ["native", "web"]
  },
  "dependencies": [
    "@plain/dep",
    { "name": "@other/heavy", "features": ["native"] }
  ]
}
```

- `rewatch build` at `@example/lib` — compiles every source dir; `@other/heavy` builds with just its `native` feature because that's all the consumer requested; `@plain/dep` builds with all of its features.
- `rewatch build --features all-backends` — compiles `src`, `src-native`, `src-web`; skips `src-experimental`.
- `rewatch build --features experimental` — compiles `src`, `src-experimental`; skips the backends.
