(** Declarations collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access) *)

(* Both types have the same representation, but different semantics *)
type t = Decl.t PosHash.t
type builder = Decl.t PosHash.t

(* ===== Builder API ===== *)

let create_builder () : builder = PosHash.create 256

let add (builder : builder) (pos : Lexing.position) (decl : Decl.t) =
  PosHash.replace builder pos decl

let find_opt_builder (builder : builder) pos = PosHash.find_opt builder pos

let replace_builder (builder : builder) (pos : Lexing.position) (decl : Decl.t)
    =
  PosHash.replace builder pos decl

let merge_all (builders : builder list) : t =
  let result = PosHash.create 256 in
  builders
  |> List.iter (fun builder ->
         PosHash.iter (fun pos decl -> PosHash.replace result pos decl) builder);
  result

(* ===== Read-only API ===== *)

let find_opt (t : t) pos = PosHash.find_opt t pos

let fold f (t : t) init = PosHash.fold f t init

let iter f (t : t) = PosHash.iter f t
