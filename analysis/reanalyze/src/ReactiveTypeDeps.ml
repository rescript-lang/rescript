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
  (* refs_to direction: target -> sources *)
  same_path_refs: (Lexing.position, PosSet.t) Reactive.t;
  cross_file_refs: (Lexing.position, PosSet.t) Reactive.t;
  all_type_refs: (Lexing.position, PosSet.t) Reactive.t;
  impl_to_intf_refs_path2: (Lexing.position, PosSet.t) Reactive.t;
  intf_to_impl_refs: (Lexing.position, PosSet.t) Reactive.t;
  (* refs_from direction: source -> targets (for forward solver) *)
  all_type_refs_from: (Lexing.position, PosSet.t) Reactive.t;
}
(** All reactive collections for type-label dependencies *)

(** Create reactive type-label dependency collections from a decls collection *)
let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(report_types_dead_only_in_interface : bool) : t =
  (* Step 1: Index decls by path *)
  let decl_by_path =
    Reactive.flatMap ~name:"type_deps.decl_by_path" decls
      ~f:(fun _pos decl ->
        match decl_to_info decl with
        | Some info -> [(info.path, [info])]
        | None -> [])
      ~merge:List.append ()
  in

  (* Step 2: Same-path refs - connect all decls at the same path *)
  let same_path_refs =
    Reactive.flatMap ~name:"type_deps.same_path_refs" decl_by_path
      ~f:(fun _path decls ->
        match decls with
        | [] | [_] -> []
        | first :: rest ->
          (* Connect each decl to the first one (and vice-versa if needed).
             Original: extendTypeDependencies loc loc0 adds posTo=loc, posFrom=loc0
             So: posTo=other, posFrom=first *)
          rest
          |> List.concat_map (fun other ->
                 (* Always add: other -> first (posTo=other, posFrom=first) *)
                 let refs = [(other.pos, PosSet.singleton first.pos)] in
                 if report_types_dead_only_in_interface then refs
                 else
                   (* Also add: first -> other (posTo=first, posFrom=other) *)
                   (first.pos, PosSet.singleton other.pos) :: refs))
      ~merge:PosSet.union ()
  in

  (* Step 3: Cross-file refs - connect impl decls to intf decls *)
  (* First, extract impl decls that need to look up intf *)
  let impl_decls =
    Reactive.flatMap ~name:"type_deps.impl_decls" decls
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

  (* Join impl decls with decl_by_path to find intf.
     Original: extendTypeDependencies loc loc1 where loc=impl, loc1=intf
               adds posTo=impl, posFrom=intf *)
  let impl_to_intf_refs =
    Reactive.join ~name:"type_deps.impl_to_intf_refs" impl_decls decl_by_path
      ~key_of:(fun _pos (_, intf_path1, _) -> intf_path1)
      ~f:(fun _pos (info, _intf_path1, _intf_path2) intf_decls_opt ->
        match intf_decls_opt with
        | Some (intf_info :: _) ->
          (* Found at path1: posTo=impl, posFrom=intf *)
          let refs = [(info.pos, PosSet.singleton intf_info.pos)] in
          if report_types_dead_only_in_interface then refs
          else
            (* Also: posTo=intf, posFrom=impl *)
            (intf_info.pos, PosSet.singleton info.pos) :: refs
        | _ -> [])
      ~merge:PosSet.union ()
  in

  (* Second join for path2 fallback *)
  let impl_needing_path2 =
    Reactive.join ~name:"type_deps.impl_needing_path2" impl_decls decl_by_path
      ~key_of:(fun _pos (_, intf_path1, _) -> intf_path1)
      ~f:(fun pos (info, _intf_path1, intf_path2) intf_decls_opt ->
        match intf_decls_opt with
        | Some (_ :: _) -> [] (* Found at path1, skip *)
        | _ -> [(pos, (info, intf_path2))])
      ()
  in

  let impl_to_intf_refs_path2 =
    Reactive.join ~name:"type_deps.impl_to_intf_refs_path2" impl_needing_path2
      decl_by_path
      ~key_of:(fun _pos (_, intf_path2) -> intf_path2)
      ~f:(fun _pos (info, _) intf_decls_opt ->
        match intf_decls_opt with
        | Some (intf_info :: _) ->
          (* posTo=impl, posFrom=intf *)
          let refs = [(info.pos, PosSet.singleton intf_info.pos)] in
          if report_types_dead_only_in_interface then refs
          else (intf_info.pos, PosSet.singleton info.pos) :: refs
        | _ -> [])
      ~merge:PosSet.union ()
  in

  (* Also handle intf -> impl direction.
     Original: extendTypeDependencies loc1 loc where loc=impl, loc1=intf
               adds posTo=impl, posFrom=intf (note: same direction!)
     The intf->impl code in original only runs when isInterface=true,
     and the lookup is for finding the impl. *)
  let intf_decls =
    Reactive.flatMap ~name:"type_deps.intf_decls" decls
      ~f:(fun _pos decl ->
        match decl_to_info decl with
        | Some info when info.is_interface -> (
          match info.path with
          | [] -> []
          | typeLabelName :: pathToType ->
            let impl_path =
              typeLabelName :: DcePath.moduleToImplementation pathToType
            in
            [(info.pos, (info, impl_path))])
        | _ -> [])
      ()
  in

  let intf_to_impl_refs =
    Reactive.join ~name:"type_deps.intf_to_impl_refs" intf_decls decl_by_path
      ~key_of:(fun _pos (_, impl_path) -> impl_path)
      ~f:(fun _pos (intf_info, _) impl_decls_opt ->
        match impl_decls_opt with
        | Some (impl_info :: _) ->
          (* Original: extendTypeDependencies loc1 loc where loc1=intf, loc=impl
             But wait, looking at the original code more carefully:
             
             if isInterface then
               match find_one path1 with
               | None -> ()
               | Some loc1 ->
                 extendTypeDependencies ~config ~refs loc1 loc;
                 if not Config.reportTypesDeadOnlyInInterface then
                   extendTypeDependencies ~config ~refs loc loc1
             
             Here loc is the current intf decl, loc1 is the found impl.
             So extendTypeDependencies loc1 loc means posTo=loc1=impl, posFrom=loc=intf
          *)
          let refs = [(impl_info.pos, PosSet.singleton intf_info.pos)] in
          if report_types_dead_only_in_interface then refs
          else (intf_info.pos, PosSet.singleton impl_info.pos) :: refs
        | _ -> [])
      ~merge:PosSet.union ()
  in

  (* Cross-file refs are the combination of:
     - impl_to_intf_refs (path1 matches)
     - impl_to_intf_refs_path2 (path2 fallback)
     - intf_to_impl_refs *)
  let cross_file_refs = impl_to_intf_refs in

  (* All type refs = same_path_refs + all cross-file sources.
     We expose these separately and merge in freeze_refs. *)
  let all_type_refs = same_path_refs in

  (* Create refs_from by combining and inverting all refs_to sources.
     We use a single flatMap that iterates all sources once. *)
  let all_type_refs_from =
    (* Combine all refs_to sources using union *)
    let combined_refs_to =
      let u1 =
        Reactive.union ~name:"type_deps.u1" same_path_refs cross_file_refs
          ~merge:PosSet.union ()
      in
      let u2 =
        Reactive.union ~name:"type_deps.u2" u1 impl_to_intf_refs_path2
          ~merge:PosSet.union ()
      in
      Reactive.union ~name:"type_deps.combined_refs_to" u2 intf_to_impl_refs
        ~merge:PosSet.union ()
    in
    (* Invert the combined refs_to to refs_from *)
    Reactive.flatMap ~name:"type_deps.all_type_refs_from" combined_refs_to
      ~f:(fun posTo posFromSet ->
        PosSet.elements posFromSet
        |> List.map (fun posFrom -> (posFrom, PosSet.singleton posTo)))
      ~merge:PosSet.union ()
  in

  {
    decl_by_path;
    same_path_refs;
    cross_file_refs;
    all_type_refs;
    impl_to_intf_refs_path2;
    intf_to_impl_refs;
    all_type_refs_from;
  }

(** {1 Freezing for solver} *)

(** Add all type refs to a References.builder *)
let add_to_refs_builder (t : t) ~(refs : References.builder) : unit =
  Reactive.iter
    (fun posTo posFromSet ->
      PosSet.iter
        (fun posFrom -> References.add_type_ref refs ~posTo ~posFrom)
        posFromSet)
    t.all_type_refs
