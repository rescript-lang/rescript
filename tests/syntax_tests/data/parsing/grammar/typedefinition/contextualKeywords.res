// Type names
type open = int
type as<'a> = 'a
type rec = int
type rec rec = list<rec>
type of = A
type when = {when: int}

// Fields named mutable
type t = {mutable: bool, mutable?: int, mutable mutable: string}
type t = {foo: string, mutable}

// Aliases of a type named private
type t = (private)
type t = (private as 'a)
type t = (private<int>)
type t = private private
module X: S with type t = (private) = M

// Labels and arrow types
type fn = (~as: int, ~open: bool=?) => unit
type t = as => of

// The keywords are unchanged
type t<'a> = 'a constraint 'a = int
type t = private int
type t = private {x: int}
type t =
  private
  | A
  | B
type t = [#a] as 'x
type rec t = A(t)
external open: int => int = "open"
