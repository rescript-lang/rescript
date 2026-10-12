type special = | @as("special") SpecialKind
@unboxed
type rec witness<'a> =
  | Number({kind: string}): witness<int>
  | Text({kind: special}): witness<string>

@inline(never) let id = value => value
let extract:
  type a. witness<a> => a =
  value =>
    switch value {
    | Number(_) => 42
    | Text(_) => "oops"
    }
let result: int = extract(id(Number({kind: "special"})))
