type t = _
type t = node<_>

type t = {name: _}
type t = {name: @attr _}

type t = Plant(_)
type t = Plant(@attr _)

type t = (_, _) => unit
type t = (@attr _, @attr _) => unit

let x: _ = ()
let x: @attr _ = ()

external foo: _ = "foo_c_binding"
external foo: @attr _ = "foo_c_binding"
