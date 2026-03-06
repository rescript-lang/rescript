(** Reactive mapping from declarations to their outgoing references.
    
    This is the reactive version of [Liveness.build_decl_refs_index].
    
    For each declaration, computes the set of positions it references.
    Updates incrementally when refs or declarations change. *)

(** Build reactive index: decl_pos -> (value_targets, type_targets)
    
    Uses pure reactive combinators - no internal hashtables. *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(value_refs_from : (Lexing.position, PosSet.t) Reactive.t)
    ~(type_refs_from : (Lexing.position, PosSet.t) Reactive.t) :
    (Lexing.position, PosSet.t * PosSet.t) Reactive.t =
  (* Group declarations by file *)
  let decls_by_file : (string, (Lexing.position * Decl.t) list) Reactive.t =
    Reactive.FlatMap.create ~name:"decl_refs.decls_by_file" decls
      ~f:(fun pos decl emit -> emit pos.Lexing.pos_fname [(pos, decl)])
      ~merge:( @ ) ()
  in

  (* Check if posFrom is contained in decl's range *)
  let pos_in_decl (posFrom : Lexing.position) (decl : Decl.t) : bool =
    posFrom.pos_fname = decl.pos.pos_fname
    && posFrom.pos_cnum >= decl.posStart.pos_cnum
    && posFrom.pos_cnum <= decl.posEnd.pos_cnum
  in

  (* For each ref, find which decl(s) contain it and output (decl_pos, targets) *)
  let value_decl_refs : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Join.create ~name:"decl_refs.value_decl_refs" value_refs_from
      decls_by_file
      ~key_of:(fun posFrom _targets -> posFrom.Lexing.pos_fname)
      ~f:(fun posFrom targets decls_mb emit ->
        if ReactiveMaybe.is_some decls_mb then
          let decls_in_file = ReactiveMaybe.unsafe_get decls_mb in
          List.iter
            (fun (decl_pos, decl) ->
              if pos_in_decl posFrom decl then emit decl_pos targets)
            decls_in_file)
      ~merge:PosSet.union ()
  in

  let type_decl_refs : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Join.create ~name:"decl_refs.type_decl_refs" type_refs_from
      decls_by_file
      ~key_of:(fun posFrom _targets -> posFrom.Lexing.pos_fname)
      ~f:(fun posFrom targets decls_mb emit ->
        if ReactiveMaybe.is_some decls_mb then
          let decls_in_file = ReactiveMaybe.unsafe_get decls_mb in
          List.iter
            (fun (decl_pos, decl) ->
              if pos_in_decl posFrom decl then emit decl_pos targets)
            decls_in_file)
      ~merge:PosSet.union ()
  in

  (* Combine value and type refs into (value_targets, type_targets) pairs.
     Use join to combine, with decls as the base to ensure all decls are present. *)
  let with_value_refs : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Join.create ~name:"decl_refs.with_value_refs" decls value_decl_refs
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos _decl refs_mb emit ->
        let refs =
          if ReactiveMaybe.is_some refs_mb then ReactiveMaybe.unsafe_get refs_mb
          else PosSet.empty
        in
        emit pos refs)
      ()
  in

  let with_type_refs : (Lexing.position, PosSet.t) Reactive.t =
    Reactive.Join.create ~name:"decl_refs.with_type_refs" decls type_decl_refs
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos _decl refs_mb emit ->
        let refs =
          if ReactiveMaybe.is_some refs_mb then ReactiveMaybe.unsafe_get refs_mb
          else PosSet.empty
        in
        emit pos refs)
      ()
  in

  (* Combine into final (value_targets, type_targets) pairs *)
  Reactive.Join.create ~name:"decl_refs.combined" with_value_refs with_type_refs
    ~key_of:(fun pos _value_targets -> pos)
    ~f:(fun pos value_targets type_targets_mb emit ->
      let type_targets =
        if ReactiveMaybe.is_some type_targets_mb then
          ReactiveMaybe.unsafe_get type_targets_mb
        else PosSet.empty
      in
      emit pos (value_targets, type_targets))
    ()
