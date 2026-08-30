# Third-party license inventory

`THIRD_PARTY_LICENSES` is generated separately for every native platform npm
package. It is intentionally not distributed with the root `rescript` package,
`@rescript/runtime`, or `@rescript/belt`, because those packages contain no
native binaries.

The generated file has two inputs:

- `curated-components.json` and the referenced license snapshots cover
  vendored OCaml sources, linked OCaml libraries, and platform toolchains.
- `about.toml` and `about.hbs` generate a target-specific Rust dependency
  section from `rewatch/Cargo.lock` using cargo-about 0.9.2.

After changing a vendored component, an OCaml dependency, a toolchain, or
`rewatch/Cargo.lock`, update the corresponding inventory and run:

```sh
yarn licenses:generate
```

`yarn licenses:generate` writes the target-specific notices directly into the
native platform package directories. `yarn licenses:check` verifies those
generated files. They are ignored by Git and regenerated before publishing.
