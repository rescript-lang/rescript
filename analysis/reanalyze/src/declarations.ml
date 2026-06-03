(** Declarations collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access) *)

(* Both types have the same representation, but different semantics *)
type t = Decl.t Pos_hash.t
type builder = Decl.t Pos_hash.t

(* ===== Builder API ===== *)

let create_builder () : builder = Pos_hash.create 256

let add (builder : builder) (pos : Lexing.position) (decl : Decl.t) =
  Pos_hash.replace builder pos decl

let find_opt_builder (builder : builder) pos = Pos_hash.find_opt builder pos

let replace_builder (builder : builder) (pos : Lexing.position) (decl : Decl.t)
    =
  Pos_hash.replace builder pos decl

let merge_all (builders : builder list) : t =
  let result = Pos_hash.create 256 in
  builders
  |> List.iter (fun builder ->
         Pos_hash.iter
           (fun pos decl -> Pos_hash.replace result pos decl)
           builder);
  result

(* ===== Builder extraction for reactive merge ===== *)

let builder_to_list (builder : builder) : (Lexing.position * Decl.t) list =
  Pos_hash.fold (fun pos decl acc -> (pos, decl) :: acc) builder []

let create_from_hashtbl (h : Decl.t Pos_hash.t) : t = h

(* ===== Read-only API ===== *)

let find_opt (t : t) pos = Pos_hash.find_opt t pos

let fold f (t : t) init = Pos_hash.fold f t init

let iter f (t : t) = Pos_hash.iter f t

let length (t : t) = Pos_hash.length t
