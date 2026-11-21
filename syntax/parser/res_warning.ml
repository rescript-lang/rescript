type kind = Deprecated_uncurried_dot

type t = {loc: Location.t; kind: kind}

let message = function
  | Deprecated_uncurried_dot ->
    "The `(. ...)` uncurried syntax is deprecated. Uncurried is now the \
     default in ReScript — remove the leading dot."
