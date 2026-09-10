(** Annotation lookup over the reactive collection of per-position annotations. *)

type t = (Lexing.position, File_annotations.annotated_as) Reactive.t

let of_reactive reactive = reactive

let is_annotated_dead t pos = Reactive.get t pos = Some File_annotations.Dead

let is_annotated_gentype_or_live t pos =
  match Reactive.get t pos with
  | Some (File_annotations.Live | File_annotations.GenType) -> true
  | Some File_annotations.Dead | None -> false

let is_annotated_gentype_or_dead t pos =
  match Reactive.get t pos with
  | Some (File_annotations.Dead | File_annotations.GenType) -> true
  | Some File_annotations.Live | None -> false
