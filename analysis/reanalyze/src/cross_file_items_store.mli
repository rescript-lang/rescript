(** Abstraction over cross-file items storage.

    Allows iteration over optional arg calls and function refs from either:
    - [Frozen]: Collected [CrossFileItems.t] 
    - [Reactive]: Direct iteration over reactive collection (no intermediate allocation) *)

type t =
  | Frozen of Cross_file_items.t
  | Reactive of (string, Cross_file_items.t) Reactive.t
      (** Cross-file items store with exposed constructors for pattern matching *)

val of_frozen : Cross_file_items.t -> t
(** Wrap a frozen [CrossFileItems.t] *)

val of_reactive : (string, Cross_file_items.t) Reactive.t -> t
(** Wrap reactive collection directly (no intermediate collection) *)

val iter_optional_arg_calls :
  t -> (Cross_file_items.optional_arg_call -> unit) -> unit
(** Iterate over all optional arg calls *)

val iter_function_refs : t -> (Cross_file_items.function_ref -> unit) -> unit
(** Iterate over all function refs *)

val compute_optional_args_state :
  t ->
  find_decl:(Lexing.position -> Decl.t option) ->
  is_live:(Lexing.position -> bool) ->
  Optional_args_state.t
(** Compute optional args state from calls and function references *)

val compute_live_direct_optional_arg_calls :
  t ->
  find_decl:(Lexing.position -> Decl.t option) ->
  is_live:(Lexing.position -> bool) ->
  Pos_set.t
(** Compute live optional-arg call targets that hit reporting declarations directly *)
