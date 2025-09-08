/* include ( module X : sig val fib : int -> int end = struct let rec fib = function */
/* | 0 | 1 -> 1 */
/* | n -> fib (n - 1) + fib (n - 2) */

/* let u = List.length */
/* end ) */

/* type u = Leaf | Node of u * u */

/* let rec sum = function */
/* | Leaf -> 1 */
/* | Node (l,r)  -> sum l + sum r */

/* let leaf = Leaf */
/* let node l r = Node (l,r) */

/* let f = Math.abs 3.0 
Original definition at externs.zip//es3.js:888
var Math=require("Math");
*/
/* import List from "List" */
/* let g f x =  x |> f */

/* raise Not_found;; */ /* Test alone next time */
let a0 = __LOC__
let a1 = __MODULE__
let a2 = __LINE__
let a3 = __MODULE__
let a4 = __LOC_OF__
let a5 = __LINE_OF__
let a6 = __POS_OF__
let test_float = float(3)
let test_abs = abs_float(3.0)
/* Clflags.dump_lambda:=true */
let v = [1.0, 2.0]
let xxx = "a"
let a = String.get(xxx, 0)
let u = b => {
  let a = if b {
    Js.log(1)
    32
  } else {
    3 + 4
  }
  a
}
let f = (h, b, ()) =>
  h(
    if b {
      Js.log(1)
      32
    } else {
      3 + 4
    },
  )
let f2 = (h, b, ()) =>
  h(
    if b {
      32
    } else {
      3 + 4
    },
  )
@@warning("-26")
let f2 = (h, b, ()) =>
  h(
    if b {
      let c = 3
      32
    } else {
      3 + 4
    },
  )
let () = v[1] = 3.0

type u = {
  mutable u: float,
  v: float,
}
let xx = (0., 0.)
let unboxed_x = {u: 0., v: 0.}
let f = ({u}) => u

let gg = x => x.u = 0.

let f = (x: string) => String.length(x)

let is_lazy_force = x => Lazy.get(x)

let rec fib = (n: int) =>
  switch n {
  | 0 | 1 => 1
  | n =>
    let fib1 = fib(n - 1)
    let fib2 = fib(n - 2)
    let fib3 = 3
    fib1 + fib2 + fib3
  }
