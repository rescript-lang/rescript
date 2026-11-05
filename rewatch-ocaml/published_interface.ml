let own_identity filename contents =
  match Cmi_format.read_crcs_and_flags filename contents with
  | (_, crc) :: _, flags -> Some (crc, flags)
  | [], _ -> None
  | exception Cmi_format.Error _ -> None

let publish ~source ~destination =
  let contents = File_util.read_build_file source in
  match Build_file_store.read destination with
  | Some previous when previous = contents -> Build_state.Cmi_unchanged
  | Some previous
    when let identity = own_identity source contents in
         Option.is_some identity && identity = own_identity destination previous
    ->
    let modified = (Unix.stat destination).Unix.st_mtime in
    File_util.write_build_file destination contents;
    Unix.utimes destination modified modified;
    Build_state.Cmi_unchanged
  | Some _ | None ->
    File_util.write_build_file destination contents;
    Build_state.Cmi_changed

let crcs path =
  fst (Cmi_format.read_crcs_and_flags path (File_util.read_build_file path))

let recorded_imports ~cmi_path ~has_interface =
  let interface_imports =
    match crcs cmi_path with
    | _own :: imports -> imports
    | [] -> []
  in
  if has_interface then (
    let cmt_path = Filename.remove_extension cmi_path ^ ".cmt" in
    let contents = File_util.read_build_file cmt_path in
    (* A typed tree is large and read once, so the store does not keep it. *)
    Build_file_store.evict cmt_path;
    interface_imports
    @ (Cmt_format.read_cmt_contents cmt_path contents).cmt_imports)
  else interface_imports

let imports_changed ~published ~cmi_path ~has_interface =
  let changed (unit, recorded) =
    match (recorded, published unit) with
    | Some _, Some path -> (
      match crcs path with
      | (_, current) :: _ -> current <> recorded
      | [] -> true)
    | None, _ | _, None -> false
  in
  try List.exists changed (recorded_imports ~cmi_path ~has_interface)
  with
  | Unix.Unix_error _ | Sys_error _ | Cmi_format.Error _ | Cmt_format.Error _ ->
    true
