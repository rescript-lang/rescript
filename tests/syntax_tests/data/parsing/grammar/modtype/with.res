module type A = Foo with type t = string
module type A = Foo with type Bar.t = string
module type A = Foo with type t<'a> = string
module type A = Foo with type t<'a, 'b> = string
module type A = Foo with type t = 'st constraint 'st = int
module type A = Foo with type t = 'st constraint 'st = int constraint 'x = int
module type A = Foo
  with type t = 'st constraint 'st = int constraint 'x = int
   and type t = 'st constraint 'st = int constraint 'x = int
   and type t = 'st constraint 'st = int constraint 'x = int
module type A = Foo with type t = private string

module type A = Foo with type t := string
module type A = Foo with type t<'a> := string
module type A = Foo with type t<'a, 'b> := string
module type A = Foo with type Bar.t<'a, 'b> := string
module type A = Foo
  with type Bar.t<'a, 'b> := string
   and type Bar.t<'a, 'b> := string
   and type Bar.t<'a, 'b> := string

module type A = Foo with module Bar = Array
module type A = Foo with module Bar = Belt.Array
module type A = Foo with module X.Bar = Belt.Array
module type A = Foo
  with module X.Bar = Belt.Array
   and module X.Bar = Belt.Array
   and module X.Bar = Belt.Array

module type A = Foo with module Bar := Array
module type A = Foo with module Bar := Belt.Array
module type A = Foo
  with module X.Bar := Belt.Array
   and module X.Bar := Belt.Array
   and module X.Bar := Belt.Array

module type Printable = {
  type t
  let print: (Format.formatter, t) => unit
}

module type Comparable = {
  type t
  let compare: (t, t) => int
}

module type PrintableComparable = {
  include Printable
  include Comparable with type t := t
}
