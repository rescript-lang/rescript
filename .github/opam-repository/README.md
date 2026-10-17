# ReScript opam overrides

This repository contains narrowly scoped dependency fixes needed by CI before
they are available from the main opam repository.

The Luv 0.5.14 override preserves the ARM64 musl compiler's
`-mno-outline-atomics` requirement while building vendored libuv. Without it,
the static executable fails to link because libuv refers to glibc's internal
`__getauxval` symbol. The patch should be contributed upstream and this
override removed once a fixed Luv release is available.

Luv 0.5.14 also vendors libuv 1.48.0. Evaluating and contributing an update to
a current libuv 1.x release is a separate upstream follow-up; changing the
vendored library is intentionally outside the OCaml rewatch port.
