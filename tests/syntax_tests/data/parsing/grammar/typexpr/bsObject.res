type t = {"age": int}
type t = {"age": int,} // trailing comma
type t = {"age": int, "name": string}
type t = {"age": int, "name": string,} // trailing comma

type t = {
  @attr
  "age": int,
  @attr2
  "name": @onTypeString string,
}

type t = {.}
type t = private {.}
type t = {..}
