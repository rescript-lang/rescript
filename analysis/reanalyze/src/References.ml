(** References collected during dead code analysis.
    
    Two types are provided:
    - [builder] - mutable, for AST processing
    - [t] - immutable, for solver (read-only access) *)

(* Position set - same definition as DeadCommon.PosSet *)
module PosSet = Set.Make (struct
  type t = Lexing.position

  let compare = compare
end)

(* Position-keyed hashtable *)
module PosHash = Hashtbl.Make (struct
  type t = Lexing.position

  let hash x =
    let s = Filename.basename x.Lexing.pos_fname in
    Hashtbl.hash (x.Lexing.pos_cnum, s)

  let equal (x : t) y = x = y
end)

(* Helper to add to a set in a hashtable *)
let addSet h k v =
  let set = try PosHash.find h k with Not_found -> PosSet.empty in
  PosHash.replace h k (PosSet.add v set)

(* Helper to find a set in a hashtable *)
let findSet h k = try PosHash.find h k with Not_found -> PosSet.empty

(* Internal representation: two hashtables *)
type refs_table = PosSet.t PosHash.t

type builder = {value_refs: refs_table; type_refs: refs_table}

type t = {value_refs: refs_table; type_refs: refs_table}

(* ===== Builder API ===== *)

let create_builder () : builder =
  {value_refs = PosHash.create 256; type_refs = PosHash.create 256}

let add_value_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.value_refs posTo posFrom

let add_type_ref (builder : builder) ~posTo ~posFrom =
  addSet builder.type_refs posTo posFrom

let merge_into_builder ~(from : builder) ~(into : builder) =
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun fromPos -> addSet into.value_refs pos fromPos))
    from.value_refs;
  PosHash.iter
    (fun pos refs ->
      refs |> PosSet.iter (fun fromPos -> addSet into.type_refs pos fromPos))
    from.type_refs

let merge_all (builders : builder list) : t =
  let result = create_builder () in
  builders
  |> List.iter (fun builder -> merge_into_builder ~from:builder ~into:result);
  {value_refs = result.value_refs; type_refs = result.type_refs}

let freeze_builder (builder : builder) : t =
  (* Zero-copy freeze - builder should not be used after this *)
  {value_refs = builder.value_refs; type_refs = builder.type_refs}

(* ===== Read-only API ===== *)

let find_value_refs (t : t) pos = findSet t.value_refs pos

let find_type_refs (t : t) pos = findSet t.type_refs pos
