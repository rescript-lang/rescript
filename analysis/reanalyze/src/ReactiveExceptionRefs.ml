(** Reactive exception reference resolution.

    Expresses exception ref resolution as a reactive join:
    - exception_refs: (path, loc_from) from CrossFileItems
    - exception_decls: (path, loc_to) indexed from Declarations
    - result: value refs (pos_to, pos_from)
    
    When declarations or exception_refs change, only affected refs update. *)

(** {1 Types} *)

type t = {
  exception_decls: (DcePath.t, Location.t) Reactive.t;
  resolved_refs: (Lexing.position, PosSet.t) Reactive.t;
}
(** Reactive exception ref collections *)

(** {1 Creation} *)

(** Create reactive exception refs from decls and cross-file exception refs.
    
    [decls] is the reactive declarations collection.
    [exception_refs] is the reactive collection of (path, loc_from) from CrossFileItems. *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(exception_refs : (DcePath.t, Location.t) Reactive.t) : t =
  (* Step 1: Index exception declarations by path *)
  let exception_decls =
    Reactive.flatMap decls
      ~f:(fun _pos (decl : Decl.t) ->
        match decl.Decl.declKind with
        | Exception ->
          let loc : Location.t =
            {
              Location.loc_start = decl.pos;
              loc_end = decl.posEnd;
              loc_ghost = false;
            }
          in
          [(decl.path, loc)]
        | _ -> [])
      () (* Last-write-wins is fine since paths should be unique *)
  in

  (* Step 2: Join exception_refs with exception_decls *)
  let resolved_refs =
    Reactive.join exception_refs exception_decls
      ~key_of:(fun path _loc_from -> path)
      ~f:(fun _path loc_from loc_to_opt ->
        match loc_to_opt with
        | Some loc_to ->
          (* Add value reference: pos_to -> pos_from *)
          [
            ( loc_to.Location.loc_start,
              PosSet.singleton loc_from.Location.loc_start );
          ]
        | None -> [])
      ~merge:PosSet.union ()
  in

  {exception_decls; resolved_refs}

(** {1 Freezing} *)

(** Add all resolved exception refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      PosSet.iter
        (fun posFrom -> References.add_value_ref refs ~posTo ~posFrom)
        posFromSet)
    t.resolved_refs

(** Add file dependencies for resolved refs *)
let add_to_file_deps_builder (t : t) ~(file_deps : FileDeps.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      PosSet.iter
        (fun posFrom ->
          let from_file = posFrom.Lexing.pos_fname in
          let to_file = posTo.Lexing.pos_fname in
          if from_file <> to_file then
            FileDeps.add_dep file_deps ~from_file ~to_file)
        posFromSet)
    t.resolved_refs
