module Curry = {}
module Block = {}

@val external map: (array<'a>, 'a => 'b) => array<'b> = "Array.prototype.map.call"

type id = int => int
@val external map2: (array<int>, int => int) => array<int> = "Array.prototype.map.cal"

/* [n] should not be documented,
    since such inconsistency could not be checked
*/

/* if we know the return value of type we could do more optimizations here */
let xbs = map([1, 2, 3, 5], x => x + 1)

let f = (cb: int => int) => map([1, 2, 3, 4], cb)

let xs = map([(1, 2), (1, 2), (2, 1)], ((x, y)) => y + x + 1)

@val external map2: (array<'a>, array<'b>, ('a, 'b) => 'c) => array<'c> = "map2"

@val external ff: (int, @ignore int, (int, int) => int) => int = "ff"

@val external ff1: (int, @as(3) _, (int, int) => int) => int = "ff1"

@val external ff2: (int, @as("3") _, (int, int) => int) => int = "ff2"

@val external hi: (unit => unit) => int = "hi"

/*
fun (_){
    f 0
}
*/

let f_0 = () => hi(() => ())
let f_01 = () =>
  hi((() as x) =>
    if x == () {
      Js.log("x")
    }
  ) /* FIXME: not inlined */
let f_02 = xs =>
  hi((() as x) => {
    xs := x
    Js.log("x")
  })
let f_03 = (xs, u) => hi(u)
/* arity adjust to [0] [ function (){return u (0)}] */

let h = (x, y, z) => map2(x, y, z)

let h1 = (x, y, u, z) => map2(x, y, z(u))

let add3 = x => (y, z) => x + y + z

let h2 = x => ff(x, 2, \"+")

let h3 = x => ff(x, 2, add3(1))

let h4 = x => ff1(x, add3(1))

let h5 = x => ff2(x, add3(2))
let add = (x, y) => {
  Js.log((x, y))
  x + y
}

let h6 = x => ff2(x, add)

type elem
@val
external optional_cb: (
  (string, ~props: elem=?, array<int>) => elem /* This should emit a warning ? */,
  string,
) => int = "optional_cb"

/*
let fishy_unit = fun () -> Js.log 1 

let fishy_unit_2 = fun [@bs] (() as x) -> Js.log x

let v = fishy_unit_2 () [@bs]

*/
/* Up is not a valid syntax, since you can not apply correctly */

/* ^ should be an error instead of warning */

/*
external ff : 
    int -> 
    (unit -> unit) -> 
    int = 
    ""
[@@val]
*/

/*
So if we pass 
{[ (fun (() as x) ->  Js.log x  ) ]}

Then we call it on JS side 
[g ()], we are passing undefined 
to [x] which is incorrect.

We can also blame users 
[()=>] is really not representable  in OCaml 
You are writing the wrong FFI.... 

They need (fun ()[@bs] -> ..)
Maybe we can create a sugar 
{[ begin [@bs]  .... end  ]} 
*/

/*
external config : 
    hi: (int -> int) ->    
    lo: int -> 
    unit -> 
    _ = "" [@@obj]

type expected = 
    < hi : int -> int [@bs]; 
      lo : int >  

let f : expected = 
    config 
    ~hi:(fun x -> x + 1 )
    ~lo:3 
    ()
*/
/*
" we auto-uncurry 
so the inferred type would be 
*/
/*
let v = ref 0  



(**
There is a semantics mismatch when converting curried function into uncurried function 
for example 
`let u = f a b c in u d ` may have a side effect here when creating [u].
We should document it clearly
*)
let a4 = Js.Internal.js_fn_mk4 (fun x y z -> incr v ;  fun d -> 1 + d)


let () = 
    ignore @@ a4 0 1 2 3 [@bs]
    ignore @@ a4 0 1 2 3 [@bs]

;;

Mt.from_pair_suites __MODULE__ !suites
*/

let unit_magic = () => {
  Js.log("noinline")
  Js.log("noinline")
  3
}

let f_unit_magic = unit_magic()

@variadic @val external f_0002: (string, array<int>) => unit = "f_0002"

let hh = (xs, a) => f_0002(xs, a)
