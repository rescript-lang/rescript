<div align="center">

   <h3> <a href="https://rescript-lang.org"> ReScript Documentation </a> | <a href="https://forum.rescript-lang.org/"> Forum </a> | <a href="https://rescript-lang.org.cn/community/roadmap#roadmap"> Roadmap</a>  | <a href="https://rescript-lang.org.cn/"> 中文 </a> | <a href="https://green-labs.github.io/rescript-in-korean/"> 한국어 </a> </h3>
  
</div>

## Overview

The compiler for ReScript: a statically typed functional language focused on shipping.

[![npm version](https://badge.fury.io/js/rescript.svg)](https://badge.fury.io/js/rescript) ![Build Status](https://circleci.com/gh/rescript-lang/rescript-compiler.svg?style=svg)

## Installation

```sh
npm install --save-dev rescript
```

## Contribution

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Acknowledgments

- Thanks to the [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this project would not exist. The original type system was inherited from [it](jscomp/ml) with lots of modifications to fit with JS ecosystem better.
- Thanks to [ninja-build](https://ninja-build.org), ReScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool
- Thanks to [Bloomberg](https://www.techatbloomberg.com) and [Facebook](https://github.com/facebook/). This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. This project's funded by Facebook since July 2017

## History

This project was originally created by [Hongbo Zhang](https://github.com/bobzhang) in 2015 and
still actively maintained by Hongbo Zhang hosted at
[here](https://github.com/rescript-lang/rescript-compiler)

It was named BuckleScript and rebranded into [ReScript](https://rescript-lang.org/) in 2020.
The major contributions from contributors include super_errors from
[Cheng](https://github.com/chenglou) and [Cristiano](https://github.com/cristianoc), react_jsx_ppx from [Ricky](https://github.com/rickyvetter).
Cristiano also contributed to several important patches in the upstream native compiler,
in particular, the pattern match compilation.

More details are available [here](https://github.com/rescript-lang/rescript-compiler/graphs/contributors).

## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

`vendor/ocaml.tar.gz` contains the official [OCaml](https://ocaml.org) compiler (version 4.14.0).
Refer to its copyright and license notices for information about its licensing.

`vendor/ninja.tar.gz` contains the vendored [ninja](https://github.com/ninja-build/ninja).
Refer to its copyright and license notices for information about its licensing.

Note that OSS is for sharing of knowledge, but the authorship should be respected. Copyright headers in each file, Acknowledgements and History section in this file should be kept **intact**.

See [Credits](./CREDITS.md) for more details.
