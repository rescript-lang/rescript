(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access)
    
    References are stored in BOTH directions:
    - refs_to: posTo -> {posFrom1, posFrom2, ...} = who references posTo
    - refs_from: posFrom -> {posTo1, posTo2, ...} = what posFrom references
    
    This allows gradual migration from backward to forward algorithms. *)

(* Helper to add to a set in a hashtable *)
let addSet h k v =
  let set = try PosHash.find h k with Not_found -> PosSet.empty in
  PosHash.replace h k (PosSet.add v set)

(* Helper to find a set in a hashtable *)
let findSet h k = try PosHash.find h k with Not_found -> PosSet.empty

(* Internal representation: four hashtables (two directions x two ref types) *)
type refs_table = PosSet.t PosHash.t

type builder = {
  (* refs_to direction: posTo -> {sources that reference it} *)
  value_refs_to: refs_table;
  type_refs_to: refs_table;
  (* refs_from direction: posFrom -> {targets it references} *)
  value_refs_from: refs_table;
  type_refs_from: refs_table;
}

type t = {
  value_refs_to: refs_table;
  type_refs_to: refs_table;
  value_refs_from: refs_table;
  type_refs_from: refs_table;
}

(* ===== Builder API ===== *)

let create_builder () : builder =
  {
    value_refs_to = PosHash.create 256;
    type_refs_to = PosHash.create 256;
    value_refs_from = PosHash.create 256;
    type_refs_from = PosHash.create 256;
  }

(* Store in both directions *)
let add_value_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.value_refs_to posTo posFrom;
  addSet builder.value_refs_from posFrom posTo

let add_type_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.type_refs_to posTo posFrom;
  addSet builder.type_refs_from posFrom posTo

let merge_into_builder ~(from : builder) ~(into : builder) =
  (* Merge refs_to direction *)
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun fromPos -> addSet into.value_refs_to pos fromPos))
    from.value_refs_to;
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun fromPos -> addSet into.type_refs_to pos fromPos))
    from.type_refs_to;
  (* Merge refs_from direction *)
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
    value_refs_to = result.value_refs_to;
    type_refs_to = result.type_refs_to;
    value_refs_from = result.value_refs_from;
    type_refs_from = result.type_refs_from;
  }

let freeze_builder (builder : builder) : t =
  (* Zero-copy freeze - builder should not be used after this *)
  {
    value_refs_to = builder.value_refs_to;
    type_refs_to = builder.type_refs_to;
    value_refs_from = builder.value_refs_from;
    type_refs_from = builder.type_refs_from;
  }

(* ===== Builder extraction for reactive merge ===== *)

(* Extract refs_to direction (posTo -> {sources}) *)
let builder_value_refs_to_list (builder : builder) :
    (Lexing.position * PosSet.t) list =
  PosHash.fold (fun pos refs acc -> (pos, refs) :: acc) builder.value_refs_to []

let builder_type_refs_to_list (builder : builder) :
    (Lexing.position * PosSet.t) list =
  PosHash.fold (fun pos refs acc -> (pos, refs) :: acc) builder.type_refs_to []

(* Extract refs_from direction (posFrom -> {targets}) *)
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

let create ~value_refs_to ~type_refs_to ~value_refs_from ~type_refs_from : t =
  {value_refs_to; type_refs_to; value_refs_from; type_refs_from}

(* ===== Read-only API ===== *)

(* refs_to direction: find who references this position (for reporting) *)
let find_value_refs (t : t) pos = findSet t.value_refs_to pos
let find_type_refs (t : t) pos = findSet t.type_refs_to pos

let value_refs_length (t : t) = PosHash.length t.value_refs_to
let type_refs_length (t : t) = PosHash.length t.type_refs_to

(* refs_from direction: iterate over what positions reference (for liveness) *)
let iter_value_refs_from (t : t) f = PosHash.iter f t.value_refs_from
let iter_type_refs_from (t : t) f = PosHash.iter f t.type_refs_from
