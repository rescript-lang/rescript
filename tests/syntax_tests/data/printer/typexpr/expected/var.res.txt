type t = 'a
type t = @attr 'a

type t = constr<'a>

external foo: 'foo = "primitive"
external foo: @attr 'foo = "primitive"

let x: 'a = y
let x: @attr 'a = y

type x<'A> = 'A
