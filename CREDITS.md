# History

This project was originally created by [Hongbo Zhang](https://github.com/bobzhang) in 2015. It was named BuckleScript and rebranded into [ReScript](https://rescript-lang.org/) in 2020.

The major contributions from contributors include

- `super_errors` from [Cheng](https://github.com/chenglou) and [Cristiano](https://github.com/cristianoc)
- `react_jsx_ppx` from [Ricky](https://github.com/rickyvetter)
- `rewatch` (the new ReScript build system) from [Roland](https://github.com/rolandpeelen) and [Jaap](https://github.com/jfrolich)

Cristiano also contributed to several important patches in the upstream native compiler,
in particular, the pattern match compilation.

More details are available [here](https://github.com/rescript-lang/rescript-compiler/graphs/contributors).

# Acknowledgments

## OCaml

Thanks to the [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this project would not exist.

ReScript builds on these parts of OCaml:

- [`compiler/core/lam_pass_exits.ml`](compiler/core/lam_pass_exits.ml)
- [`compiler/core/lam_pass_lets_dce.ml`](compiler/core/lam_pass_lets_dce.ml)

These modules were adapted from [`ocaml/bytecomp/simplif.ml`](ocaml/bytecomp/simplif.ml) for
JavaScript specific optimization purposes.

- [`compiler/main/rescript_compiler_main.ml`](compiler/main/rescript_compiler_main.ml)

`compiler/main/rescript_compiler_main.ml` is adapted from [`ocaml/driver/main.ml`](ocaml/driver/main.ml). It is the main entry
point of the underlying compiler.

ReScript imported one file from [jsoo](https://github.com/ocsigen/js_of_ocaml)

- [`compiler/core/js_dump.ml`](compiler/core/js_dump.ml) (pretty printer)

This file was imported but changed significantly in iterations later.

- [`tests/tests`](tests/tests)

`tests/tests` is based on [`ocaml/testsuite`](ocaml/testsuite).

ReScript compiler unit test builds on [OUnit](http://ounit.forge.ocamlcore.org/)

## Ninja

Thanks to [ninja-build](https://ninja-build.org), ReScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool.

## Bloomberg and Facebook

Thanks to [Bloomberg](https://www.techatbloomberg.com) and [Facebook](https://github.com/facebook/). This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. This project's funded by Facebook since July 2017.
