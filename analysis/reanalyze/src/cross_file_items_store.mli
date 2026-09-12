(** Iteration over cross-file items held in the reactive collection. *)

type t
(** Abstract cross-file items store *)

val of_reactive : (string, Cross_file_items.t) Reactive.t -> t
(** Wrap the reactive collection (no intermediate collection) *)

val iter_optional_arg_calls :
  t -> (Cross_file_items.optional_arg_call -> unit) -> unit
(** Iterate over all optional arg calls *)

val iter_function_refs : t -> (Cross_file_items.function_ref -> unit) -> unit
(** Iterate over all function refs *)

val iter_optional_arg_value_escapes :
  t -> (Cross_file_items.optional_arg_value_escape -> unit) -> unit
(** Iterate over optional-arg functions used as first-class values *)

val compute_optional_args_state :
  t ->
  find_decl:(Lexing.position -> Decl.t option) ->
  is_live:(Lexing.position -> bool) ->
  Optional_args_state.t
(** Compute optional args state from calls and function references *)

val compute_live_optional_arg_value_escapes :
  t -> is_live:(Lexing.position -> bool) -> Pos_set.t
(** Compute optional-arg declarations with live first-class value escapes *)
