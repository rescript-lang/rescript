(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in refs_from direction only:
    - refs_from: posFrom -> {posTo1, posTo2, ...} = what posFrom references
    
    This is what the forward liveness algorithm needs. *)

(* Helper to add to a set in a hashtable *)
let add_set h k v =
  let set = try Pos_hash.find h k with Not_found -> Pos_set.empty in
  Pos_hash.replace h k (Pos_set.add v set)

(* Internal representation: two hashtables (refs_from for value and type) *)
type refs_table = Pos_set.t Pos_hash.t

type builder = {value_refs_from: refs_table; type_refs_from: refs_table}

(* ===== Builder API ===== *)

let create_builder () : builder =
  {value_refs_from = Pos_hash.create 256; type_refs_from = Pos_hash.create 256}

let add_value_ref (builder : builder) ~pos_to ~pos_from =
  add_set builder.value_refs_from pos_from pos_to

let add_type_ref (builder : builder) ~pos_to ~pos_from =
  add_set builder.type_refs_from pos_from pos_to

let builder_value_refs_from_list (builder : builder) :
    (Lexing.position * Pos_set.t) list =
  Pos_hash.fold
    (fun pos refs acc -> (pos, refs) :: acc)
    builder.value_refs_from []

let builder_type_refs_from_list (builder : builder) :
    (Lexing.position * Pos_set.t) list =
  Pos_hash.fold
    (fun pos refs acc -> (pos, refs) :: acc)
    builder.type_refs_from []
