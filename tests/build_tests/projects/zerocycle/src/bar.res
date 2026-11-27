// one-file false positive: https://github.com/rescript-lang/rescript-compiler/issues/5368

module Nested = {
  module Bar = {
    type t = private int
  }
}

open Nested

module Bar = {
  open Bar
  let t: t = Obj.magic(42)
}
