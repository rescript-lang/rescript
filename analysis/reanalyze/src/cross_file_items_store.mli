(** Iteration over cross-file items held in the reactive collection. *)

type t
(** Abstract cross-file items store *)

val of_reactive : (string, Cross_file_items.t) Reactive.t -> t
(** Wrap the reactive collection (no intermediate collection) *)

val compute_optional_args_state :
  t ->
  find_decl:(Lexing.position -> Decl.t option) ->
  is_live:(Lexing.position -> bool) ->
  Optional_args_state.t
(** Compute optional args state from calls and function references *)

val compute_live_optional_arg_value_escapes :
  t -> is_live:(Lexing.position -> bool) -> Pos_set.t
(** Compute optional-arg declarations with live first-class value escapes *)
