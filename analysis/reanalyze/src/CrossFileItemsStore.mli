(** Abstraction over cross-file items storage.

    Allows iteration over optional arg calls and function refs from either:
    - [Frozen]: Collected [CrossFileItems.t] 
    - [Reactive]: Direct iteration over reactive collection (no intermediate allocation) *)

type t =
  | Frozen of CrossFileItems.t
  | Reactive of (string, CrossFileItems.t) Reactive.t
      (** Cross-file items store with exposed constructors for pattern matching *)

val of_frozen : CrossFileItems.t -> t
(** Wrap a frozen [CrossFileItems.t] *)

val of_reactive : (string, CrossFileItems.t) Reactive.t -> t
(** Wrap reactive collection directly (no intermediate collection) *)

val iter_optional_arg_calls :
  t -> (CrossFileItems.optional_arg_call -> unit) -> unit
(** Iterate over all optional arg calls *)

val iter_function_refs : t -> (CrossFileItems.function_ref -> unit) -> unit
(** Iterate over all function refs *)

val compute_optional_args_state :
  t ->
  find_decl:(Lexing.position -> Decl.t option) ->
  is_live:(Lexing.position -> bool) ->
  OptionalArgsState.t
(** Compute optional args state from calls and function references *)
