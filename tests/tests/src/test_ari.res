let f = (x, y) => x + y
/*
{[
 f = function (x){ function(x,y){x + y} (x) }
]}
  after optimization
{[
  f = \x -> \y -> ..
]}
actually will become 
{[
  f = \(x,y) ->  x + y 
]}

*/
let f1 = (x, y) => x + y
let f3 = (g, x) => g(x)
let f2 = \"+"(3, ...)
let g = f(3, 4)

@val("test_primit") @module("U") external ext: (int, int) => int = "test_primit"

let ff = ext(3, ...)
type u = int => int
@val("test_primit2") @module("VV") external ext: int => u = "test_primit2"
let fff = ext(3)

let rec length_aux = (len, x) =>
  switch x {
  | list{} => len
  | list{a, ...l} => length_aux(len + 1, l)
  }

include Belt.List
