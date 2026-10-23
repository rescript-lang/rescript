type rec witness<'a> =
  | Int: witness<int>
  | String: witness<string>

let defaultValue:
  type a. witness<a> => a =
  witness =>
    switch witness {
    | Int => 42
    | String => "value"
    }

let intDefault: int = defaultValue(Int)
let stringDefault: string = defaultValue(String)
