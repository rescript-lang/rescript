/* include (struct */
@module("./File.js") external foo: int = "foo"
/* end : sig val foo : int end ) */

@module("./File.js") external foo2: int => int = "foo2"

let bar = foo

module Nested = {
  @module("./File.js") external nested: int => int = "nested"
}

module Make = (
  X: {
    let v: int
  },
) => {
  @module("./File.js") external inFunctor: int => int = "inFunctor"
  let w = inFunctor(X.v)
}

@module("pkg") external notRelative: int => int = "notRelative"

module rec Recursive: {
  @module("./File.js") external inRec: int => int = "inRec"
  let call: int => int
} = {
  @module("./File.js") external inRec: int => int = "inRec"
  let call = x => inRec(x)
}
