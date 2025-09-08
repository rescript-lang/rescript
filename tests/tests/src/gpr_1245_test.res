let (a, b) = (1, 2)

let (x, y) = (ref(a), ref(b))

let f = ((c, d)) => {
  let (a, b) = (ref(c), ref(d))
  Js.log2(a, b)
}

/* escape analysis to avoid allocation
    {[
      let match/xx = [a,b,c]
      ]}
    later if [match/xx] is only used in destructring
    [Pfield match/xx i] then there is no need to construct 
    such block.

    It is more general than   
    [Js.Null.toOption]
    since its box number is one and immutable,
    so we can give it a meaningful name for such slot

    This also applies to [reference]
    exception allocation
*/

type t = {x: int, y: int}

let g = () => {
  let z = {x: 1, y: 2}
  z.x + z.y
}

let a0 = f => {
  let u = Js.Null.toOption(f())
  switch u {
  | None => 0
  | Some(x) =>
    Js.log(x)
    Js.log(x)
    1
  }
}

let a1 = f => {
  module M = {
    exception E
  }
  try f() catch {
  | M.E => 1
  }
}
