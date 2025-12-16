(** Reactive type-label dependencies.

    Expresses the type-label dependency computation as a reactive pipeline:
    1. decls -> decl_by_path (index by path)
    2. decl_by_path -> same_path_refs (connect duplicates at same path)
    3. decl_by_path + impl_decls -> cross_file_refs (connect impl<->intf)
    
    When declarations change, only affected refs are recomputed. *)

(** {1 Helper types} *)

type decl_info = {
  pos: Lexing.position;
  pos_end: Lexing.position;
  path: DcePath.t;
  is_interface: bool;
}
(** Simplified decl info for type-label processing *)

let decl_to_info (decl : Decl.t) : decl_info option =
  match decl.declKind with
  | RecordLabel | VariantCase ->
    let is_interface =
      match List.rev decl.path with
      | [] -> true
      | moduleNameTag :: _ -> (
        try (moduleNameTag |> Name.toString).[0] <> '+' with _ -> true)
    in
    Some {pos = decl.pos; pos_end = decl.posEnd; path = decl.path; is_interface}
  | _ -> None

(** {1 Reactive Collections} *)

type t = {
  decl_by_path: (DcePath.t, decl_info list) Reactive.t;
  same_path_refs: (Lexing.position, PosSet.t) Reactive.t;
  cross_file_refs: (Lexing.position, PosSet.t) Reactive.t;
  all_type_refs: (Lexing.position, PosSet.t) Reactive.t;
}
(** All reactive collections for type-label dependencies *)

(** Create reactive type-label dependency collections from a decls collection *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(report_types_dead_only_in_interface : bool) : t =
  (* Step 1: Index decls by path *)
  let decl_by_path =
    Reactive.flatMap decls
      ~f:(fun _pos decl ->
        match decl_to_info decl with
        | Some info -> [(info.path, [info])]
        | None -> [])
      ~merge:List.append ()
  in

  (* Step 2: Same-path refs - connect all decls at the same path *)
  let same_path_refs =
    Reactive.flatMap decl_by_path
      ~f:(fun _path decls ->
        match decls with
        | [] | [_] -> []
        | first :: rest ->
          (* Connect each decl to the first one (and vice-versa if needed) *)
          rest
          |> List.concat_map (fun other ->
                 let refs =
                   [(first.pos, PosSet.singleton other.pos);
                    (other.pos, PosSet.singleton first.pos)]
                 in
                 if report_types_dead_only_in_interface then
                   (* Only first -> other *)
                   [(other.pos, PosSet.singleton first.pos)]
                 else refs))
      ~merge:PosSet.union ()
  in

  (* Step 3: Cross-file refs - connect impl decls to intf decls *)
  (* First, extract impl decls that need to look up intf *)
  let impl_decls =
    Reactive.flatMap decls
      ~f:(fun _pos decl ->
        match decl_to_info decl with
        | Some info when not info.is_interface -> (
          match info.path with
          | [] -> []
          | typeLabelName :: pathToType ->
            (* Try two intf paths *)
            let path_1 = pathToType |> DcePath.moduleToInterface in
            let path_2 = path_1 |> DcePath.typeToInterface in
            let intf_path1 = typeLabelName :: path_1 in
            let intf_path2 = typeLabelName :: path_2 in
            [(info.pos, (info, intf_path1, intf_path2))])
        | _ -> [])
      ()
  in

  (* Join impl decls with decl_by_path to find intf *)
  let impl_to_intf_refs =
    Reactive.join impl_decls decl_by_path
      ~key_of:(fun _pos (_, intf_path1, _) -> intf_path1)
      ~f:(fun _pos (info, _intf_path1, intf_path2) intf_decls_opt ->
        match intf_decls_opt with
        | Some (intf_info :: _) ->
          (* Found at path1, connect impl <-> intf *)
          if report_types_dead_only_in_interface then
            [(intf_info.pos, PosSet.singleton info.pos)]
          else
            [(info.pos, PosSet.singleton intf_info.pos);
             (intf_info.pos, PosSet.singleton info.pos)]
        | _ ->
          (* Try path2 - need second join, but for now return placeholder *)
          (* We'll handle path2 with a separate join below *)
          [(info.pos, (intf_path2, info))] |> List.filter_map (fun _ -> None))
      ~merge:PosSet.union ()
  in

  (* Second join for path2 fallback *)
  let impl_needing_path2 =
    Reactive.join impl_decls decl_by_path
      ~key_of:(fun _pos (_, intf_path1, _) -> intf_path1)
      ~f:(fun pos (info, _intf_path1, intf_path2) intf_decls_opt ->
        match intf_decls_opt with
        | Some (_ :: _) -> [] (* Found at path1, skip *)
        | _ -> [(pos, (info, intf_path2))])
      ()
  in

  let impl_to_intf_refs_path2 =
    Reactive.join impl_needing_path2 decl_by_path
      ~key_of:(fun _pos (_, intf_path2) -> intf_path2)
      ~f:(fun _pos (info, _) intf_decls_opt ->
        match intf_decls_opt with
        | Some (intf_info :: _) ->
          if report_types_dead_only_in_interface then
            [(intf_info.pos, PosSet.singleton info.pos)]
          else
            [(info.pos, PosSet.singleton intf_info.pos);
             (intf_info.pos, PosSet.singleton info.pos)]
        | _ -> [])
      ~merge:PosSet.union ()
  in

  (* Also handle intf -> impl direction *)
  let intf_decls =
    Reactive.flatMap decls
      ~f:(fun _pos decl ->
        match decl_to_info decl with
        | Some info when info.is_interface -> (
          match info.path with
          | [] -> []
          | typeLabelName :: pathToType ->
            let impl_path = typeLabelName :: DcePath.moduleToImplementation pathToType in
            [(info.pos, (info, impl_path))])
        | _ -> [])
      ()
  in

  let intf_to_impl_refs =
    Reactive.join intf_decls decl_by_path
      ~key_of:(fun _pos (_, impl_path) -> impl_path)
      ~f:(fun _pos (info, _) impl_decls_opt ->
        match impl_decls_opt with
        | Some (impl_info :: _) ->
          if report_types_dead_only_in_interface then
            [(info.pos, PosSet.singleton impl_info.pos)]
          else
            [(impl_info.pos, PosSet.singleton info.pos);
             (info.pos, PosSet.singleton impl_info.pos)]
        | _ -> [])
      ~merge:PosSet.union ()
  in

  (* Combine all cross-file refs *)
  let cross_file_refs =
    Reactive.flatMap impl_to_intf_refs
      ~f:(fun pos refs -> [(pos, refs)])
      ~merge:PosSet.union ()
  in
  (* Merge in path2 refs *)
  let cross_file_refs =
    Reactive.flatMap impl_to_intf_refs_path2
      ~f:(fun pos refs -> [(pos, refs)])
      ~merge:PosSet.union ()
    |> fun refs2 ->
    Reactive.flatMap cross_file_refs
      ~f:(fun pos refs ->
        let additional =
          match Reactive.get refs2 pos with
          | Some r -> r
          | None -> PosSet.empty
        in
        [(pos, PosSet.union refs additional)])
      ~merge:PosSet.union ()
  in
  (* Merge in intf->impl refs *)
  let cross_file_refs =
    Reactive.flatMap intf_to_impl_refs
      ~f:(fun pos refs -> [(pos, refs)])
      ~merge:PosSet.union ()
    |> fun refs3 ->
    Reactive.flatMap cross_file_refs
      ~f:(fun pos refs ->
        let additional =
          match Reactive.get refs3 pos with
          | Some r -> r
          | None -> PosSet.empty
        in
        [(pos, PosSet.union refs additional)])
      ~merge:PosSet.union ()
  in

  (* Step 4: Combine same-path and cross-file refs *)
  let all_type_refs =
    Reactive.flatMap same_path_refs
      ~f:(fun pos refs ->
        let cross =
          match Reactive.get cross_file_refs pos with
          | Some r -> r
          | None -> PosSet.empty
        in
        [(pos, PosSet.union refs cross)])
      ~merge:PosSet.union ()
  in
  (* Also include cross-file refs that don't have same-path refs *)
  let all_type_refs =
    Reactive.flatMap cross_file_refs
      ~f:(fun pos refs ->
        match Reactive.get same_path_refs pos with
        | Some _ -> [] (* Already included above *)
        | None -> [(pos, refs)])
      ~merge:PosSet.union ()
    |> fun extra_refs ->
    Reactive.flatMap all_type_refs
      ~f:(fun pos refs ->
        let extra =
          match Reactive.get extra_refs pos with
          | Some r -> r
          | None -> PosSet.empty
        in
        [(pos, PosSet.union refs extra)])
      ~merge:PosSet.union ()
  in

  {decl_by_path; same_path_refs; cross_file_refs; all_type_refs}

(** {1 Freezing for solver} *)

(** Add all type refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      PosSet.iter
        (fun posFrom -> References.add_type_ref refs ~posTo ~posFrom)
        posFromSet)
    t.all_type_refs

