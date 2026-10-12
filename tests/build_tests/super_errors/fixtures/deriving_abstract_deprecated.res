@deriving(abstract)
type t = {name: string}

@deriving({abstract: light})
type u = {age: int}

module type S = {
  @deriving(abstract)
  type t = {name: string}
}
