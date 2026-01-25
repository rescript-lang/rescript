(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in refs_from direction only:
    - refs_from: posFrom -> {posTo1, posTo2, ...} = what posFrom references
    
    This is what the forward liveness algorithm needs. *)

(* Helper to add to a set in a hashtable *)
let addSet h k v =
  let set = try PosHash.find h k with Not_found -> PosSet.empty in
  PosHash.replace h k (PosSet.add v set)

(* Internal representation: two hashtables (refs_from for value and type) *)
type refs_table = PosSet.t PosHash.t

type builder = {value_refs_from: refs_table; type_refs_from: refs_table}

type t = {value_refs_from: refs_table; type_refs_from: refs_table}

(* ===== Builder API ===== *)

let create_builder () : builder =
  {value_refs_from = PosHash.create 256; type_refs_from = PosHash.create 256}

let add_value_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.value_refs_from posFrom posTo

let add_type_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.type_refs_from posFrom posTo

let merge_into_builder ~(from : builder) ~(into : builder) =
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun toPos -> addSet into.value_refs_from pos toPos))
    from.value_refs_from;
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun toPos -> addSet into.type_refs_from pos toPos))
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
    (Lexing.position * PosSet.t) list =
  PosHash.fold
    (fun pos refs acc -> (pos, refs) :: acc)
    builder.value_refs_from []

let builder_type_refs_from_list (builder : builder) :
    (Lexing.position * PosSet.t) list =
  PosHash.fold
    (fun pos refs acc -> (pos, refs) :: acc)
    builder.type_refs_from []

let create ~value_refs_from ~type_refs_from : t =
  {value_refs_from; type_refs_from}

(* ===== Read-only API ===== *)

let iter_value_refs_from (t : t) f = PosHash.iter f t.value_refs_from
let iter_type_refs_from (t : t) f = PosHash.iter f t.type_refs_from

let value_refs_from_length (t : t) = PosHash.length t.value_refs_from
let type_refs_from_length (t : t) = PosHash.length t.type_refs_from
