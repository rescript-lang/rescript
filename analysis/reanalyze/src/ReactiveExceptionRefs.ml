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
  resolved_refs_from: (Lexing.position, PosSet.t) Reactive.t;
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
    Reactive.FlatMap.create ~name:"exc_refs.exception_decls" decls
      ~f:(fun _pos decl wave ->
        let decl : Decl.t = Stable.to_linear_value decl in
        match decl.Decl.declKind with
        | Exception ->
          let loc : Location.t =
            {
              Location.loc_start = decl.pos;
              loc_end = decl.posEnd;
              loc_ghost = false;
            }
          in
          StableWave.push wave
            (Stable.unsafe_of_value decl.path)
            (Stable.unsafe_of_value loc)
        | _ -> ())
      () (* Last-write-wins is fine since paths should be unique *)
  in

  (* Step 2: Join exception_refs with exception_decls *)
  let resolved_refs =
    Reactive.Join.create ~name:"exc_refs.resolved_refs" exception_refs
      exception_decls
      ~key_of:(fun path _loc_from -> path)
      ~f:(fun _path loc_from loc_to_mb wave ->
        let loc_from = Stable.to_linear_value loc_from in
        if Maybe.is_some loc_to_mb then
          let loc_to = Stable.to_linear_value (Maybe.unsafe_get loc_to_mb) in
          (* Add value reference: pos_to -> pos_from (refs_to direction) *)
          StableWave.push wave
            (Stable.unsafe_of_value loc_to.Location.loc_start)
            (Stable.unsafe_of_value
               (PosSet.singleton loc_from.Location.loc_start)))
      ~merge:(fun a b ->
        Stable.unsafe_of_value
          (PosSet.union (Stable.to_linear_value a) (Stable.to_linear_value b)))
      ()
  in

  (* Step 3: Create refs_from direction by inverting *)
  let resolved_refs_from =
    Reactive.FlatMap.create ~name:"exc_refs.resolved_refs_from" resolved_refs
      ~f:(fun posTo posFromSet wave ->
        let posTo = Stable.to_linear_value posTo in
        let posFromSet = Stable.to_linear_value posFromSet in
        PosSet.iter
          (fun posFrom ->
            StableWave.push wave
              (Stable.unsafe_of_value posFrom)
              (Stable.unsafe_of_value (PosSet.singleton posTo)))
          posFromSet)
      ~merge:(fun a b ->
        Stable.unsafe_of_value
          (PosSet.union (Stable.to_linear_value a) (Stable.to_linear_value b)))
      ()
  in

  {exception_decls; resolved_refs; resolved_refs_from}

(** {1 Freezing} *)

(** Add all resolved exception refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      let posTo = Stable.to_linear_value posTo in
      PosSet.iter
        (fun posFrom -> References.add_value_ref refs ~posTo ~posFrom)
        (Stable.to_linear_value posFromSet))
    t.resolved_refs

(** Add file dependencies for resolved refs *)
let add_to_file_deps_builder (t : t) ~(file_deps : FileDeps.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      let posTo = Stable.to_linear_value posTo in
      PosSet.iter
        (fun posFrom ->
          let from_file = posFrom.Lexing.pos_fname in
          let to_file = posTo.Lexing.pos_fname in
          if from_file <> to_file then
            FileDeps.add_dep file_deps ~from_file ~to_file)
        (Stable.to_linear_value posFromSet))
    t.resolved_refs
