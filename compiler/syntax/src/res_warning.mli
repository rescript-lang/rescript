(** The warnings the parser reports. The parser returns them with its result;
    the compiler turns them into its own warnings. *)

type kind =
  | Deprecated_uncurried_dot
      (** The legacy [(. ...)] uncurried syntax. Uncurried is the default
          since ReScript v11, so the dot no longer means anything. *)

type t = {loc: Location.t; kind: kind}

val message : kind -> string
