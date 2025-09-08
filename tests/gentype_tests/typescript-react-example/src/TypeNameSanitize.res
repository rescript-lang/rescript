@genType
type t' = int

module M = {
  @genType
  type t'' = int
}

@genType
type pair = (t', M.t'')
