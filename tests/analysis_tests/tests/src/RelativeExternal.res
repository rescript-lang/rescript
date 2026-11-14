// An external with a relative path is exported as a value.
@module("./local") external relative: int => int = "relative"
//                            ^hov

let a = relative(1)
//       ^hov

let b = relative(2)
//        ^ref

module Nested = {
  @module("./local") external nested: int => int = "nested"
}

let c = Nested.nested(3)
//               ^def
