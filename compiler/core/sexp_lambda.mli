(* Sexp printer for Lambda IR *)

val print_lambda : Format.formatter -> Lam.t -> unit
val print_lambda_with_locs : Format.formatter -> Lam.t -> unit
