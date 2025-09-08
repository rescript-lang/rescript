type t = string
type t = Parser.t
type t = Lang.Parser.t
type t = option<string>
type t = option<string,>
type t = Option.t<string>
type t = Option.t<string,>
type t = Mod.Sub.t<a, b, c>
type t = Mod.Sub.t<a, b, c,>
type t = list
type t = list<string>
type t = list<{"age": int}>
type t = list<{"age": int}, {"name": string}>
type t = {..} // Note: this compiles to bucklescript
type t = list<{..}>
type t = {.. "age": int} // Note: this compiles to bucklescript
type t = list<{.. "age": int}>
type t = {.. @attr "age": int} // Note: this compiles to bucklescript
type t = list<{.. @attr "age": int}> // Note: this compiles to bucklescript
type t = {.. @attr "age": int, @attr "name": string}
type t = list<{.. @attr "age": int, @attr "name": string}>
type t = {.. "age": int,} // Note: this compiles to bucklescript
type t = list<{.. "age": int,}> // Note: this compiles to bucklescript
type t = {.. "age": int, "name": string,} // Note: this compiles to bucklescript
type t = list<{.. "age": int, "name": string,}> // Note: this compiles to bucklescript

let t: string = x 
let t: Parser.t = x
let t: Lang.Parser.t = x
let t: option<string> = x
let t: option<string,> = x
let t: Option.t<string> = x
let t: Option.t<string,> = x
let t: Mod.Sub.t<a, b, c> = x
let t: Mod.Sub.t<a, b, c,> = x
let t: list = x
let t: list<string> = x
let t: list<{"age": int}> = x
let t: list<{"age": int, }> = x
let t: list<{@attr "age": int}> = x
let t: list<{@attr "age": int,}> = x
let t: list<{@attr "age": int, @attr "name": string}> = x
let t: list<{"age": int}, {"name": string}> = x
let t: list<{@attr "age": int}, {@attr "name": string}> = x
let t: list<
  {@attr "age": int, @attr "name": string},
  {@attr "name": string, @attr "age": int}
> = x
let t: list<{..}> = x // Note: this compiles to bucklescript
let t: list<{.}> = x // Note: this compiles to bucklescript
let t: list<{.. "age": int}> = x// Note: this compiles to bucklescript
let t: list<{. "age": int}> = x// Note: this compiles to bucklescript
let t: list<{.. "age": int,}> = x// Note: this compiles to bucklescript
let t: list<{.. @attr "age": int,}> = x// Note: this compiles to bucklescript
let t: list<{. @attr "age": int,}> = x// Note: this compiles to bucklescript
let t: list<{.. @attr "age": int}> = x// Note: this compiles to bucklescript
let t: list<{.. "age": int, "name": string,}> = x // Note: this compiles to bucklescript
let t: list<{.. @attr "age": int, @attr "name": string,}> = x // Note: this compiles to bucklescript

// >= isn't an infix op
let t: list<string>= x

// capitalised type variable in record
type id_6<'T, 'E> = | Ok('T) | Err({payload: 'E})

// capitalised type variable in as pattern
let foo = (x: int as 'X) => x

// capitalised type variable in type constraint
module type A = Foo with type t = 'X constraint 'X = int
