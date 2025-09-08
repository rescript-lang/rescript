@@warning("A")
@@warnerror("a")
@internal.local
module P0 = {
  {
    let a = 3
    Js.log(a)
    (a + 2)->ignore
  }
}
open! P0

@internal.local
module P1 = {
  exception A
  let _a = 2
}
open! P1

let f = () => throw(A)

%%private(let b = 3)

%%private(let c = b + 2)

%%private(
  let d = c
  let f = d
  let h = (a, b) => a + b
)

%%private(let h0 = 1)

%%private(let h1 = h0 + 1)

%%private(let h2 = h1 + 1)

%%private(
  let h3 = 1
  let h4 = h3 + 1
  let h5 = h4 + 1
)

module N = {
  %%private(let a = 3)
  let b = a + 2
}

Js.log(h5)
Js.log(h2)

Js.log(f)

Js.log(h(1, 2))

/* module%private X  = Arg
 type x = X.spec */
/* [%%debugger.chrome] */

module H = () => {
  %%private(@module("./x") external x: int => int = "x")
}
