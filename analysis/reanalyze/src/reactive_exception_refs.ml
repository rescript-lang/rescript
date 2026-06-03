(** Reactive exception reference resolution.

    Expresses exception ref resolution as a reactive join:
    - exception_refs: (path, loc_from) from CrossFileItems
    - exception_decls: (path, loc_to) indexed from Declarations
    - result: value refs (pos_to, pos_from)
    
    When declarations or exception_refs change, only affected refs update. *)

(** {1 Types} *)

type t = {
  exception_decls: (Dce_path.t, Location.t) Reactive.t;
  resolved_refs: (Lexing.position, Pos_set.t) Reactive.t;
  resolved_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
}
(** Reactive exception ref collections *)

(** {1 Creation} *)

(** Create reactive exception refs from decls and cross-file exception refs.
    
    [decls] is the reactive declarations collection.
    [exception_refs] is the reactive collection of (path, loc_from) from CrossFileItems. *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(exception_refs : (Dce_path.t, Location.t) Reactive.t) : t =
  (* Step 1: Index exception declarations by path *)
  let exception_decls =
    Reactive.flat_map ~name:"exc_refs.exception_decls" decls
      ~f:(fun _pos (decl : Decl.t) ->
        match decl.Decl.decl_kind with
        | Exception ->
          let loc : Location.t =
            {
              Location.loc_start = decl.pos;
              loc_end = decl.pos_end;
              loc_ghost = false;
            }
          in
          [(decl.path, loc)]
        | _ -> [])
      () (* Last-write-wins is fine since paths should be unique *)
  in

  (* Step 2: Join exception_refs with exception_decls *)
  let resolved_refs =
    Reactive.join ~name:"exc_refs.resolved_refs" exception_refs exception_decls
      ~key_of:(fun path _loc_from -> path)
      ~f:(fun _path loc_from loc_to_opt ->
        match loc_to_opt with
        | Some loc_to ->
          (* Add value reference: pos_to -> pos_from (refs_to direction) *)
          [
            ( loc_to.Location.loc_start,
              Pos_set.singleton loc_from.Location.loc_start );
          ]
        | None -> [])
      ~merge:Pos_set.union ()
  in

  (* Step 3: Create refs_from direction by inverting *)
  let resolved_refs_from =
    Reactive.flat_map ~name:"exc_refs.resolved_refs_from" resolved_refs
      ~f:(fun pos_to pos_from_set ->
        Pos_set.elements pos_from_set
        |> List.map (fun pos_from -> (pos_from, Pos_set.singleton pos_to)))
      ~merge:Pos_set.union ()
  in

  {exception_decls; resolved_refs; resolved_refs_from}

(** {1 Freezing} *)

(** Add all resolved exception refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun pos_to pos_from_set ->
      Pos_set.iter
        (fun pos_from -> References.add_value_ref refs ~pos_to ~pos_from)
        pos_from_set)
    t.resolved_refs

(** Add file dependencies for resolved refs *)
let add_to_file_deps_builder (t : t) ~(file_deps : File_deps.builder) : unit =
  Reactive.iter
    (fun pos_to pos_from_set ->
      Pos_set.iter
        (fun pos_from ->
          let from_file = pos_from.Lexing.pos_fname in
          let to_file = pos_to.Lexing.pos_fname in
          if from_file <> to_file then
            File_deps.add_dep file_deps ~from_file ~to_file)
        pos_from_set)
    t.resolved_refs
