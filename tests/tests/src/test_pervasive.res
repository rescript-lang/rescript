module Pervasives = {
  include List
  include Pervasives
}

let f = List.concat

let a0 = Math.abs
let a1 = Math.acos
let a2 = Math.tan
let a3 = Math.tanh
let a4 = Math.asin
let a5 = Math.atan2
let a6 = Math.atan
let a7 = Math.ceil
let a8 = Math.cos
let a9 = Math.cosh
let a10 = Math.exp
let a11 = Math.sin
let a12 = Math.sinh
let a13 = Math.sqrt
let a14 = Math.floor
let a15 = Math.log
let a16 = Math.log10
let a17 = Math.log1p
let a18 = \"**"
/* local variables: */
/* compile-command: "ocamlc -dlambda -c test_pervasive.ml" */
/* end: */
