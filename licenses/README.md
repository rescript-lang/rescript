# Third-party license inventory

Every native platform npm package (`@rescript/<platform>`) contains a
`THIRD_PARTY_LICENSES` file with the notices for the third-party software
compiled into its binaries. The root `rescript` package, `@rescript/runtime`,
and `@rescript/belt` contain no native binaries and do not ship it.

The files are generated from `components.json` by
`scripts/generateThirdPartyLicenses.js`:

- An entry with an `opam` field is an OCaml library linked into the binaries.
  Its version and license text come from the current opam switch. The script
  asks dune which opam packages the shipped executables link, and fails when
  one is missing from the inventory or listed without being linked.
- Every other entry is vendored or adapted source, or a platform toolchain,
  with its license text in this directory or in the vendored source
  directory. Entries with a `platforms` field are included only there: Linux
  binaries are linked statically against glibc, and Windows binaries against
  the MinGW-w64 runtime and winpthreads.

After a dependency or OCaml upgrade, run in the project's opam switch:

```sh
yarn licenses:generate
```

The generated files are committed. CI checks them in the linux-x64 build job,
whose switch produces the release binaries. Update the glibc entry when the
Ubuntu runner image changes the glibc version.
