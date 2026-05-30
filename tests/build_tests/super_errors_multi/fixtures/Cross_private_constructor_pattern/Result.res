type t =
  | Loaded(string)
  | Loading

let loaded = s => Loaded(s)
