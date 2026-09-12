(** Reactive exception reference resolution.

    Expresses exception ref resolution as a reactive join.
    When declarations or exception_refs change, only affected refs update.

    {2 Pipeline}

    {[
      decls                    exception_refs
        |                           |
        | flatMap                   |
        ↓                           |
      exception_decls               |
      (path → loc)                  |
              ↘                    ↙
                    join
                      ↓
               resolved_refs
              (pos → PosSet)
    ]}

    {2 Example}

    {[
      let exc_refs = ReactiveExceptionRefs.create
        ~decls:merged.decls
        ~exception_refs:(flatMap cross_file ~f:extract_exception_refs ())
      in
      exc_refs.resolved_refs_from
    ]} *)

(** {1 Types} *)

type t = {
  exception_decls: (Dce_path.t, Location.t) Reactive.t;
  resolved_refs: (Lexing.position, Pos_set.t) Reactive.t;
      (** refs_to direction: target -> sources *)
  resolved_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
      (** refs_from direction: source -> targets (for forward solver) *)
}
(** Reactive exception ref collections *)

(** {1 Creation} *)

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  exception_refs:(Dce_path.t, Location.t) Reactive.t ->
  t
(** Create reactive exception refs from decls and cross-file exception refs.
    
    When the source collections change, resolved refs automatically update. *)
