(** Abstraction over declaration storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [Declarations.t] (copied from reactive)
    - [Reactive]: Direct [Reactive.t] (no copy, zero-cost on warm runs)

    This eliminates the O(N) freeze step when using reactive mode. *)

type t =
  | Frozen of Declarations.t
  | Reactive of (Lexing.position, Decl.t) Reactive.t
      (** Declaration store - either frozen or reactive *)

val of_frozen : Declarations.t -> t
(** Wrap a frozen [Declarations.t] *)

val of_reactive : (Lexing.position, Decl.t) Reactive.t -> t
(** Wrap a reactive collection directly (no copy) *)

val find_opt : t -> Lexing.position -> Decl.t option
(** Look up a declaration by position *)

val fold : (Lexing.position -> Decl.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over all declarations *)

val iter : (Lexing.position -> Decl.t -> unit) -> t -> unit
(** Iterate over all declarations *)
