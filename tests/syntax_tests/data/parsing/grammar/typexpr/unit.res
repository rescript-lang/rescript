type t = ()
type t = () => ()
type t = ((), ()) => ()

type t = (()) => ()

let f = (f: () => ()) => f()
let f = (f: (()) => ()) => f()
let f = (f: ((), ()) => ()) => f((), ())

external svg: () => React.element = "svg"
external thing: () => () = "svg"
