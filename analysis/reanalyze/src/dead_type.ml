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

module Path_map = Map.Make (struct
  type t = Dce_path.t

  let compare = Stdlib.compare
end)

let process_type_label_dependencies ~config ~decls ~refs ~coercions =
  (* Build an index from full label path -> list of locations *)
  let index =
    Declarations.fold
      (fun _pos decl acc ->
        match decl.Decl.decl_kind with
        | RecordLabel | VariantCase ->
          let loc = decl |> decl_raw_loc in
          let path = decl.path in
          let existing =
            Path_map.find_opt path acc |> Option.value ~default:[]
          in
          Path_map.add path (loc :: existing) acc
        | _ -> acc)
      decls Path_map.empty
  in
  (* Inner-module duplicates: if the same full path appears multiple times (e.g. from signature+structure),
     connect them together. *)
  index
  |> Path_map.iter (fun _key locs ->
      match locs with
      | [] | [_] -> ()
      | loc0 :: rest ->
        rest
        |> List.iter (fun loc ->
            extend_type_dependencies ~config ~refs loc loc0;
            if not Config.report_types_dead_only_in_interface then
              extend_type_dependencies ~config ~refs loc0 loc));

  (* Cross-file impl<->intf linking, modeled after the previous lookup logic. *)
  let hd_opt = function
    | [] -> None
    | x :: _ -> Some x
  in
  let find_one path =
    match Path_map.find_opt path index with
    | None -> None
    | Some locs -> hd_opt locs
  in

  let is_interface_of_pathToType (path_to_type : Dce_path.t) =
    match List.rev path_to_type with
    | module_name_tag :: _ -> (
      try (module_name_tag |> Name.to_string).[0] <> '+'
      with Invalid_argument _ -> true)
    | [] -> true
  in

  Declarations.iter
    (fun _pos decl ->
      match decl.Decl.decl_kind with
      | RecordLabel | VariantCase -> (
        match decl.path with
        | [] -> ()
        | type_label_name :: path_to_type -> (
          let loc = decl |> decl_raw_loc in
          let is_interface = is_interface_of_pathToType path_to_type in
          if not is_interface then
            let path_1 = path_to_type |> Dce_path.module_to_interface in
            let path_2 = path_1 |> Dce_path.type_to_interface in
            let path1 = type_label_name :: path_1 in
            let path2 = type_label_name :: path_2 in
            match find_one path1 with
            | Some loc1 ->
              extend_type_dependencies ~config ~refs loc loc1;
              if not Config.report_types_dead_only_in_interface then
                extend_type_dependencies ~config ~refs loc1 loc
            | None -> (
              match find_one path2 with
              | Some loc2 ->
                extend_type_dependencies ~config ~refs loc loc2;
                if not Config.report_types_dead_only_in_interface then
                  extend_type_dependencies ~config ~refs loc2 loc
              | None -> ())
          else
            let path_1 = path_to_type |> Dce_path.module_to_implementation in
            let path1 = type_label_name :: path_1 in
            match find_one path1 with
            | None -> ()
            | Some loc1 ->
              extend_type_dependencies ~config ~refs loc1 loc;
              if not Config.report_types_dead_only_in_interface then
                extend_type_dependencies ~config ~refs loc loc1))
      | _ -> ())
    decls;

  (* Link fields of re-exported types (type y = x = {...}) to original type fields.
     We store the manifest type path on the label declarations themselves, and
     derive the set of re-export relationships here. To preserve stable output
     ordering, we process types bottom-to-top (by their first label position)
     and fields top-to-bottom (by their label position). *)
  let compare_pos (p1 : Lexing.position) (p2 : Lexing.position) =
    match compare p1.Lexing.pos_fname p2.Lexing.pos_fname with
    | 0 -> compare p1.Lexing.pos_cnum p2.Lexing.pos_cnum
    | c -> c
  in
  (* currentTypePath -> (rep_pos, manifestTypePath, (pos, fieldName, currentLoc) list) *)
  let groups :
      ( Dce_path.t,
        Lexing.position
        * Dce_path.t
        * (Lexing.position * Name.t * Location.t) list )
      Hashtbl.t =
    Hashtbl.create 32
  in
  Declarations.iter
    (fun _pos decl ->
      match (decl.Decl.decl_kind, decl.manifest_type_path, decl.path) with
      | ( (RecordLabel | VariantCase),
          Some manifest_type_path,
          field_name :: current_type_path ) -> (
        let item = (decl.pos, field_name, decl_raw_loc decl) in
        match Hashtbl.find_opt groups current_type_path with
        | None ->
          Hashtbl.replace groups current_type_path
            (decl.pos, manifest_type_path, [item])
        | Some (rep_pos, mtp0, items) ->
          (* manifestTypePath should be stable for a given currentTypePath *)
          let rep_pos =
            if compare_pos decl.pos rep_pos < 0 then decl.pos else rep_pos
          in
          Hashtbl.replace groups current_type_path (rep_pos, mtp0, item :: items)
        )
      | _ -> ())
    decls;

  (* Record coercions: pair the labels with [pair_coercion_labels], indexing
     declarations by the type path they belong to. *)
  (if coercions <> [] then
     let labels_by_type_path =
       Declarations.fold
         (fun _pos decl acc ->
           match record_label_of_decl decl with
           | Some (type_path, label) ->
             let existing =
               Path_map.find_opt type_path acc |> Option.value ~default:[]
             in
             Path_map.add type_path (label :: existing) acc
           | None -> acc)
         decls Path_map.empty
     in
     let labels_of_candidates paths =
       paths
       |> List.concat_map (fun path ->
           Path_map.find_opt path labels_by_type_path
           |> Option.value ~default:[])
       (* Keep the pairing independent of declaration traversal order. *)
       |> List.fast_sort (fun (_, l1) (_, l2) ->
           compare_pos l1.Location.loc_start l2.Location.loc_start)
     in
     coercions |> List.fast_sort compare
     |> List.iter
          (fun {Cross_file_items.source_type_paths; target_type_paths} ->
            match labels_of_candidates source_type_paths with
            | [] -> ()
            | source_labels ->
              pair_coercion_labels ~source_labels
                ~target_labels:(labels_of_candidates target_type_paths)
                ~add_edge:(fun ~source_loc ~target_loc ->
                  extend_type_dependencies ~config ~refs source_loc target_loc)));

  groups |> Hashtbl.to_seq |> List.of_seq
  |> List.map (fun (current_type_path, (rep_pos, manifest_type_path, items)) ->
      (rep_pos, current_type_path, manifest_type_path, items))
  (* Later (lower) types first *)
  |> List.fast_sort (fun (p1, _, _, _) (p2, _, _, _) -> compare_pos p2 p1)
  |> List.iter (fun (_rep_pos, _currentTypePath, manifest_type_path, items) ->
      items
      |> List.fast_sort (fun (p1, _, _) (p2, _, _) -> compare_pos p1 p2)
      |> List.iter (fun (_pos, field_name, current_loc) ->
          let manifest_field_path = field_name :: manifest_type_path in
          match find_one manifest_field_path with
          | None -> ()
          | Some manifest_loc ->
            extend_type_dependencies ~config ~refs current_loc manifest_loc;
            extend_type_dependencies ~config ~refs manifest_loc current_loc))
