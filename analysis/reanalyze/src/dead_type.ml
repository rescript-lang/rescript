(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Dead_common

let add_type_reference ~config ~refs ~pos_from ~pos_to =
  if config.Dce_config.cli.debug then
    Log_.item "addTypeReference %s --> %s@."
      (pos_from |> Pos.to_string)
      (pos_to |> Pos.to_string);
  References.add_type_ref refs ~pos_to ~pos_from

let extend_type_dependencies ~config ~refs (loc1 : Location.t)
    (loc2 : Location.t) =
  let {Location.loc_start = pos_to; loc_ghost = ghost1} = loc1 in
  let {Location.loc_start = pos_from; loc_ghost = ghost2} = loc2 in
  if (not ghost1) && (not ghost2) && pos_to <> pos_from then (
    if config.Dce_config.cli.debug then
      Log_.item "extendTypeDependencies %s --> %s@." (pos_to |> Pos.to_string)
        (pos_from |> Pos.to_string);
    add_type_reference ~config ~refs ~pos_from ~pos_to)

let add_declaration ~config ~decls ~file ~(module_path : Module_path.t)
    ~(type_id : Ident.t) ~(type_kind : Types.type_kind)
    ~(manifest_type_path : Dce_path.t option) =
  let module_context =
    module_path.path @ [File_context.module_name_tagged file]
  in
  let path_to_type = (type_id |> Ident.name |> Name.create) :: module_context in
  let process_type_label ?(pos_adjustment = Decl.Nothing) type_label_name
      ~decl_kind ~(loc : Location.t) =
    addDeclaration_ ~config ~decls ~file ~decl_kind ~path:path_to_type ~loc
      ?manifest_type_path ~module_loc:module_path.loc ~pos_adjustment
      type_label_name
  in
  match type_kind with
  | Type_record (l, _) ->
    List.iter
      (fun {Types.ld_id; ld_loc} ->
        Ident.name ld_id |> Name.create
        |> process_type_label ~decl_kind:RecordLabel ~loc:ld_loc)
      l
  | Type_variant (decls, _) ->
    List.iteri
      (fun i {Types.cd_id; cd_loc; cd_args} ->
        let _handle_inline_records =
          match cd_args with
          | Cstr_record lbls ->
            List.iter
              (fun {Types.ld_id; ld_loc} ->
                Ident.name cd_id ^ "." ^ Ident.name ld_id
                |> Name.create
                |> process_type_label ~decl_kind:RecordLabel ~loc:ld_loc)
              lbls
          | Cstr_tuple _ -> ()
        in
        let pos_adjustment =
          (* In Res the variant loc can include the | and spaces after it *)
          let is_res =
            let fname = cd_loc.loc_start.pos_fname in
            Filename.check_suffix fname ".res"
            || Filename.check_suffix fname ".resi"
          in
          if is_res then if i = 0 then Decl.FirstVariant else OtherVariant
          else Nothing
        in
        Ident.name cd_id |> Name.create
        |> process_type_label ~decl_kind:VariantCase ~loc:cd_loc ~pos_adjustment)
      decls
  | _ -> ()

(* A record coercion [(e :> Target.t)] views the source record through the
   target's labels: reading a target label is a read of the source label of the
   same name. The edge runs target -> source only - reading a source label says
   nothing about the target's.

   The batch and the reactive pipelines share this rule and the shape of a
   record-label declaration below, and differ only in how they index
   declarations and how they record an edge. *)
(* Use raw declaration positions, not [declGetLoc], because references are keyed
   by raw positions (decl.pos). [declGetLoc] applies [posAdjustment] (e.g. +2 for
   OtherVariant), which is intended for reporting locations, not for reference
   graph keys. *)
let decl_raw_loc (decl : Decl.t) : Location.t =
  {Location.loc_start = decl.pos; loc_end = decl.pos_end; loc_ghost = false}

let record_label_of_decl (decl : Decl.t) =
  match (decl.decl_kind, decl.path) with
  | RecordLabel, label :: type_path ->
    Some (type_path, (label, decl |> decl_raw_loc))
  | _ -> None

let pair_coercion_labels ~source_labels ~target_labels ~add_edge =
  target_labels
  |> List.iter (fun (label, (target_loc : Location.t)) ->
      match List.assoc_opt label source_labels with
      | Some (source_loc : Location.t)
        when (not source_loc.loc_ghost) && (not target_loc.loc_ghost)
             && source_loc.loc_start <> target_loc.loc_start ->
        add_edge ~source_loc ~target_loc
      | _ -> ())
