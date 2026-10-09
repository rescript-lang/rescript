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

(* ===== Builder extraction for reactive merge ===== *)

let builder_to_list (builder : builder) : (Lexing.position * annotated_as) list
    =
  Pos_hash.fold (fun pos value acc -> (pos, value) :: acc) builder []
