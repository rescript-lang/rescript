(** Abstraction over annotation storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [FileAnnotations.t] (copied from reactive)
    - [Reactive]: Direct [Reactive.t] (no copy, zero-cost on warm runs) *)

type t =
  | Frozen of File_annotations.t
  | Reactive of (Lexing.position, File_annotations.annotated_as) Reactive.t

let of_frozen ann = Frozen ann

let of_reactive reactive = Reactive reactive

let is_annotated_dead t pos =
  match t with
  | Frozen ann -> File_annotations.is_annotated_dead ann pos
  | Reactive reactive -> Reactive.get reactive pos = Some File_annotations.Dead

let is_annotated_gentype_or_live t pos =
  match t with
  | Frozen ann -> File_annotations.is_annotated_gentype_or_live ann pos
  | Reactive reactive -> (
    match Reactive.get reactive pos with
    | Some (File_annotations.Live | File_annotations.GenType) -> true
    | Some File_annotations.Dead | None -> false)

let is_annotated_gentype_or_dead t pos =
  match t with
  | Frozen ann -> File_annotations.is_annotated_gentype_or_dead ann pos
  | Reactive reactive -> (
    match Reactive.get reactive pos with
    | Some (File_annotations.Dead | File_annotations.GenType) -> true
    | Some File_annotations.Live | None -> false)
