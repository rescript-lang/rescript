type t = {x: int}
type t = {mutable x: int}
// trailing comma
type t = {x: int,}
type t = {mutable x: int,}

type t = {x: int, y: int}
type t = {mutable x: int, mutable y: int}
// trailing comma
type t = {x: int, y: int,}
type t = {mutable x: int, mutable y: int}

// poly-typexpr
type t = {x: 'a 'b. polyType}

// attributes
type t = {@attr x: int, @attr2 y: int}
type t = {@rowAttr x: @onInt int, @rowAttr2 y: @onInt int}

// field-declaration punning
type t = {form}
type t = {mutable form}
type t = {form, answers}
type t = {mutable form, mutable answers}

// parses object type correct
type t = {"age": int}
type t = {..}
type t = {"age": int,}
type t = {.. "age": int}
type t = {.. "age": int, "name": string}
type t = {.. @attr "age": int}
type t = {.. @attr "age": int,}
type t = {.. @attr "age": int, @attr "name": string}
type t = {@attr "age": int}
type t = {@attr "age": int,}
type t = {@attr "age": int, "name": string}
type t = {@attr "age": int, @attr2 "name": string}

// list is accepted as record key
type domProps = {
  @optional
  label: string,
  @optional
  list: string,
  @optional
  loop: bool,
}

// attribute allowed before record representation
type t = @attr {x: int}
type t = @attr /**doc before record*/ {x: int}
