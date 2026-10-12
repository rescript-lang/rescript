@deriving(jsConverter)
type t = [#a | #b]

@deriving({accessors, jsConverter})
type u = {c: int}

@deriving({jsConverter: newType})
type v = [#e | #f]

module type S = {
  @deriving(jsConverter)
  type t = [#a | #b]
}
