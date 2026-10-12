type special = | @as("special") SpecialKind
type number = {kind: string}
type text = {kind: special}
@unboxed
type rec witness<'a> = Number(number): witness<int> | Text(text): witness<string>

@inline(never) let id = value => value
let extract:
  type a. witness<a> => a =
  value =>
    switch value {
    | Number(_) => 42
    | Text(_) => "oops"
    }
let result: int = extract(id(Number({kind: "special"})))
