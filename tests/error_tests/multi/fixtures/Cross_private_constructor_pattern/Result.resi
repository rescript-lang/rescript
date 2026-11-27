type t =
  private
  | Loaded(string)
  | Loading

let loaded: string => t
