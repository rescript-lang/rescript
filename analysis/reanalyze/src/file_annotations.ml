(** Source annotations (@dead, @live, @genType).
    
    Two types are provided:
    - [builder] - mutable, for AST processing and merging
    - [t] - immutable, for solver (read-only access) *)

type annotated_as = GenType | Dead | Live

(* Both types have the same representation, but different semantics *)
type t = annotated_as Pos_hash.t
type builder = annotated_as Pos_hash.t

(* ===== Builder API ===== *)

let create_builder () : builder = Pos_hash.create 1

let annotate_gentype (state : builder) (pos : Lexing.position) =
  Pos_hash.replace state pos GenType

let annotate_dead (state : builder) (pos : Lexing.position) =
  Pos_hash.replace state pos Dead

let annotate_live (state : builder) (pos : Lexing.position) =
  Pos_hash.replace state pos Live

let merge_all (builders : builder list) : t =
  let result = Pos_hash.create 1 in
  builders
  |> List.iter (fun builder ->
         Pos_hash.iter
           (fun pos value -> Pos_hash.replace result pos value)
           builder);
  result

(* ===== Builder extraction for reactive merge ===== *)

let builder_to_list (builder : builder) : (Lexing.position * annotated_as) list
    =
  Pos_hash.fold (fun pos value acc -> (pos, value) :: acc) builder []

let create_from_hashtbl (h : annotated_as Pos_hash.t) : t = h

(* ===== Read-only API ===== *)

let is_annotated_dead (state : t) pos = Pos_hash.find_opt state pos = Some Dead

let is_annotated_gentype_or_live (state : t) pos =
  match Pos_hash.find_opt state pos with
  | Some (Live | GenType) -> true
  | Some Dead | None -> false

let is_annotated_gentype_or_dead (state : t) pos =
  match Pos_hash.find_opt state pos with
  | Some (Dead | GenType) -> true
  | Some Live | None -> false

let length (t : t) = Pos_hash.length t

let iter f (t : t) = Pos_hash.iter f t
