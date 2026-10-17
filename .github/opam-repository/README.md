# ReScript opam overrides

This repository contains narrowly scoped dependency fixes needed by CI before
they are available from the main opam repository.

The Luv override builds Luv from a commit of the
[rescript-lang/luv](https://github.com/rescript-lang/luv) fork. The fork
vendors libuv 1.53.0 (upstream Luv 0.5.14 vendors 1.48.0) and preserves the
ARM64 musl compiler's `-mno-outline-atomics` requirement, without which the
static executable fails to link because libuv refers to glibc's internal
`__getauxval` symbol. Remove the override once a Luv release with these
changes is available.
