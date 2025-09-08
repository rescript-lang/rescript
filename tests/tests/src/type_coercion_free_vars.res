module NoFreeVars = {
  type t = private int

  let f = (x: t) => (x :> int)

  let g = (y: t) => ()

  let h = x => (g(x), (x :> int))

  //  let h2 = x => ((x :> int), g(x))
}

module WithTypeArg = {
  type t<'a> = private int

  let f = (x: t<_>) => (x :> int)
}

module FunctionType = {
  type t = private int
  let f = (_): t => Obj.magic(3)
  let _ = (f :> _ => int)
}

module Contravariant = {
  type t = private int
  let f1 = (_: int) => ()
  let _ = (f1 :> t => unit)
  let f2 = (_: int, _) => ()
  let _ = (f2 :> (t, _) => unit)
}

module Totallypoly = {
  let f = x => (x :> int)
  let idint = (x: int) => x
  let _ = f === idint
}
