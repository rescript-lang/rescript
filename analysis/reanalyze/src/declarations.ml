(** Declarations collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access) *)

(* Both types have the same representation, but different semantics *)
type builder = Decl.t Pos_hash.t

(* ===== Builder API ===== *)

let create_builder () : builder = Pos_hash.create 256

let add (builder : builder) (pos : Lexing.position) (decl : Decl.t) =
  Pos_hash.replace builder pos decl

let find_opt_builder (builder : builder) pos = Pos_hash.find_opt builder pos

let replace_builder (builder : builder) (pos : Lexing.position) (decl : Decl.t)
    =
  Pos_hash.replace builder pos decl

let builder_to_list (builder : builder) : (Lexing.position * Decl.t) list =
  Pos_hash.fold (fun pos decl acc -> (pos, decl) :: acc) builder []
