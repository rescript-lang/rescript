module Pervasives = {
  include List
  include Pervasives
}

let f = Pervasives.\"@"

let a0 = abs_float
let a1 = acos
let a2 = tan
let a3 = tanh
let a4 = asin
let a5 = atan2
let a6 = atan
let a7 = ceil
let a8 = cos
let a9 = cosh
let a10 = exp
let a11 = sin
let a12 = sinh
let a13 = sqrt
let a14 = floor
let a15 = log
let a16 = log10
let a17 = log1p
let a18 = \"**"
/* local variables: */
/* compile-command: "ocamlc -dlambda -c test_pervasive.ml" */
/* end: */
