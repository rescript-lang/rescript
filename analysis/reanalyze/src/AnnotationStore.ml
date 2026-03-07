(** Abstraction over annotation storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [FileAnnotations.t] (copied from reactive)
    - [Reactive]: Direct [Reactive.t] (no copy, zero-cost on warm runs) *)

type t =
  | Frozen of FileAnnotations.t
  | Reactive of (Lexing.position, FileAnnotations.annotated_as) Reactive.t

let of_frozen ann = Frozen ann

let of_reactive reactive = Reactive reactive

let is_annotated_dead t pos =
  match t with
  | Frozen ann -> FileAnnotations.is_annotated_dead ann pos
  | Reactive reactive ->
    let mb = Reactive.get reactive pos in
    Maybe.is_some mb && Maybe.unsafe_get mb = FileAnnotations.Dead

let is_annotated_gentype_or_live t pos =
  match t with
  | Frozen ann -> FileAnnotations.is_annotated_gentype_or_live ann pos
  | Reactive reactive ->
    let mb = Reactive.get reactive pos in
    Maybe.is_some mb
    &&
    let v = Maybe.unsafe_get mb in
    v = FileAnnotations.Live || v = FileAnnotations.GenType

let is_annotated_gentype_or_dead t pos =
  match t with
  | Frozen ann -> FileAnnotations.is_annotated_gentype_or_dead ann pos
  | Reactive reactive ->
    let mb = Reactive.get reactive pos in
    Maybe.is_some mb
    &&
    let v = Maybe.unsafe_get mb in
    v = FileAnnotations.Dead || v = FileAnnotations.GenType
