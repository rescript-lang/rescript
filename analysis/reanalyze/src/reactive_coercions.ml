(** Reactive record-coercion label linking.

    Expresses coercion resolution as reactive joins:
    - coercions: candidate source/target type paths from CrossFileItems
    - labels_by_type_path: record labels indexed by the type they belong to
    - result: type refs (pos_to = source label, pos_from = target label)

    When declarations or coercions change, only affected refs update.

    The pairing rule itself lives in [Dead_type.pair_coercion_labels], shared
    with the batch pipeline; only the indexing and the edge recording differ. *)

(** {1 Types} *)

type t = {
  labels_by_type_path: (Dce_path.t, (Name.t * Location.t) list) Reactive.t;
  resolved_refs: (Lexing.position, Pos_set.t) Reactive.t;
  resolved_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
}
(** Reactive coercion ref collections *)

(** {1 Creation} *)

(** Create reactive coercion refs from decls and cross-file coercions.

    [decls] is the reactive declarations collection.
    [coercions] is the reactive collection of coercions from CrossFileItems,
    keyed by the coercion itself so identical coercions collapse. *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(coercions : (Cross_file_items.coercion, unit) Reactive.t) : t =
  (* Step 1: Index record labels by the path of the type declaring them *)
  let labels_by_type_path =
    Reactive.flat_map ~name:"coercions.labels_by_type_path" decls
      ~f:(fun _pos decl ->
        match Dead_type.record_label_of_decl decl with
        | Some (type_path, label) -> [(type_path, [label])]
        | None -> [])
      ~merge:List.append ()
  in
  (* Step 2: Look up one side of each coercion. A coercion carries candidate
     paths because the tagging of a module path is not known when it is
     collected, so emit one lookup per candidate and union the results. *)
  let labels_of_side ~name ~paths_of =
    let lookups =
      Reactive.flat_map ~name:(name ^ ".lookups") coercions
        ~f:(fun coercion () ->
          paths_of coercion |> List.map (fun path -> (path, [coercion])))
        ~merge:List.append ()
    in
    Reactive.join ~name lookups labels_by_type_path
      ~key_of:(fun path _coercions -> path)
      ~f:(fun _path cs labels_opt ->
        match labels_opt with
        | Some labels -> cs |> List.map (fun coercion -> (coercion, labels))
        | None -> [])
      ~merge:List.append ()
  in
  let source_labels =
    labels_of_side ~name:"coercions.source_labels" ~paths_of:(fun c ->
        c.Cross_file_items.source_type_paths)
  in
  let target_labels =
    labels_of_side ~name:"coercions.target_labels" ~paths_of:(fun c ->
        c.Cross_file_items.target_type_paths)
  in
  (* Step 3: Bring both sides of a coercion together and pair the labels *)
  let resolved_refs =
    Reactive.join ~name:"coercions.resolved_refs" target_labels source_labels
      ~key_of:(fun coercion _target_labels -> coercion)
      ~f:(fun _coercion target_labels source_labels_opt ->
        match source_labels_opt with
        | None -> []
        | Some source_labels ->
          let refs = ref [] in
          Dead_type.pair_coercion_labels ~source_labels ~target_labels
            ~add_edge:(fun ~source_loc ~target_loc ->
              refs :=
                ( source_loc.Location.loc_start,
                  Pos_set.singleton target_loc.Location.loc_start )
                :: !refs);
          !refs)
      ~merge:Pos_set.union ()
  in
  (* Step 4: Create refs_from direction by inverting *)
  let resolved_refs_from =
    Reactive.flat_map ~name:"coercions.resolved_refs_from" resolved_refs
      ~f:(fun pos_to pos_from_set ->
        Pos_set.elements pos_from_set
        |> List.map (fun pos_from -> (pos_from, Pos_set.singleton pos_to)))
      ~merge:Pos_set.union ()
  in
  {labels_by_type_path; resolved_refs; resolved_refs_from}

(** {1 Freezing} *)

(** Add all resolved coercion refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun pos_to pos_from_set ->
      Pos_set.iter
        (fun pos_from -> References.add_type_ref refs ~pos_to ~pos_from)
        pos_from_set)
    t.resolved_refs
