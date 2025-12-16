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
      ReactiveExceptionRefs.add_to_refs_builder exc_refs ~refs:my_refs_builder
    ]} *)

(** {1 Types} *)

type t
(** Reactive exception ref collections *)

(** {1 Creation} *)

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  exception_refs:(DcePath.t, Location.t) Reactive.t ->
  t
(** Create reactive exception refs from decls and cross-file exception refs.
    
    When the source collections change, resolved refs automatically update. *)

(** {1 Freezing} *)

val add_to_refs_builder : t -> refs:References.builder -> unit
(** Add all resolved exception refs to a References.builder. *)

val add_to_file_deps_builder : t -> file_deps:FileDeps.builder -> unit
(** Add file dependencies for resolved refs. *)

