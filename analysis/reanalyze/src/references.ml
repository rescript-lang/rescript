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

type t = {value_refs_from: refs_table; type_refs_from: refs_table}

(* ===== Builder API ===== *)

let create_builder () : builder =
  {value_refs_from = Pos_hash.create 256; type_refs_from = Pos_hash.create 256}

let add_value_ref (builder : builder) ~pos_to ~pos_from =
  add_set builder.value_refs_from pos_from pos_to

let add_type_ref (builder : builder) ~pos_to ~pos_from =
  add_set builder.type_refs_from pos_from pos_to

let merge_into_builder ~(from : builder) ~(into : builder) =
  Pos_hash.iter
    (fun pos refs ->
      refs
      |> Pos_set.iter (fun to_pos -> add_set into.value_refs_from pos to_pos))
    from.value_refs_from;
  Pos_hash.iter
    (fun pos refs ->
      refs
      |> Pos_set.iter (fun to_pos -> add_set into.type_refs_from pos to_pos))
    from.type_refs_from

let merge_all (builders : builder list) : t =
  let result = create_builder () in
  builders
  |> List.iter (fun builder -> merge_into_builder ~from:builder ~into:result);
  {
    value_refs_from = result.value_refs_from;
    type_refs_from = result.type_refs_from;
  }

let freeze_builder (builder : builder) : t =
  (* Zero-copy freeze - builder should not be used after this *)
  {
    value_refs_from = builder.value_refs_from;
    type_refs_from = builder.type_refs_from;
  }

(* ===== Builder extraction for reactive merge ===== *)

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

let create ~value_refs_from ~type_refs_from : t =
  {value_refs_from; type_refs_from}

(* ===== Read-only API ===== *)

let iter_value_refs_from (t : t) f = Pos_hash.iter f t.value_refs_from
let iter_type_refs_from (t : t) f = Pos_hash.iter f t.type_refs_from

let value_refs_from_length (t : t) = Pos_hash.length t.value_refs_from
let type_refs_from_length (t : t) = Pos_hash.length t.type_refs_from
