(** Source annotations (@dead, @live, @genType).
    
    Two types are provided:
    - [builder] - mutable, for AST processing and merging
    - [t] - immutable, for solver (read-only access) *)

(* Position-keyed hashtable *)
module PosHash = Hashtbl.Make (struct
  type t = Lexing.position

  let hash x =
    let s = Filename.basename x.Lexing.pos_fname in
    Hashtbl.hash (x.Lexing.pos_cnum, s)

  let equal (x : t) y = x = y
end)

type annotated_as = GenType | Dead | Live

(* Both types have the same representation, but different semantics *)
type t = annotated_as PosHash.t
type builder = annotated_as PosHash.t

(* ===== Builder API ===== *)

let create_builder () : builder = PosHash.create 1

let annotate_gentype (state : builder) (pos : Lexing.position) =
  PosHash.replace state pos GenType

let annotate_dead (state : builder) (pos : Lexing.position) =
  PosHash.replace state pos Dead

let annotate_live (state : builder) (pos : Lexing.position) =
  PosHash.replace state pos Live

let merge_all (builders : builder list) : t =
  let result = PosHash.create 1 in
  builders
  |> List.iter (fun builder ->
         PosHash.iter
           (fun pos value -> PosHash.replace result pos value)
           builder);
  result

(* ===== Read-only API ===== *)

let is_annotated_dead (state : t) pos = PosHash.find_opt state pos = Some Dead

let is_annotated_gentype_or_live (state : t) pos =
  match PosHash.find_opt state pos with
  | Some (Live | GenType) -> true
  | Some Dead | None -> false

let is_annotated_gentype_or_dead (state : t) pos =
  match PosHash.find_opt state pos with
  | Some (Dead | GenType) -> true
  | Some Live | None -> false
