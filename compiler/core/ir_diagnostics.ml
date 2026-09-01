type t = {directory: string; mutable next_index: int}

let is_artifact filename =
  match Ext_filename.get_extension_maybe filename with
  | ".lam" | ".lambda" | ".jsx" -> true
  | _ -> false

let remove_stale_artifacts directory =
  Sys.readdir directory
  |> Array.iter (fun filename ->
      if is_artifact filename then
        Misc.remove_file (Filename.concat directory filename))

let create ~output_prefix =
  let directory = output_prefix ^ ".debug-ir" in
  if Sys.file_exists directory then (
    if not (Ext_sys.is_directory_no_exn directory) then
      failwith (Printf.sprintf "%s exists and is not a directory" directory);
    remove_stale_artifacts directory)
  else Sys.mkdir directory 0o755;
  Ext_log.dwarn ~__POS__ "Writing IR diagnostics to %s" directory;
  {directory; next_index = 1}

let next_path diagnostics ~kind ~pass ~extension =
  let index = diagnostics.next_index in
  diagnostics.next_index <- index + 1;
  Filename.concat diagnostics.directory
    (Printf.sprintf "%02d-%s-%s%s" index kind pass extension)

let dump_lam diagnostics ~pass lam =
  let path = next_path diagnostics ~kind:"lam" ~pass ~extension:".lam" in
  Ext_log.dwarn ~__POS__ "Dumping pass %s to %s" pass path;
  Printlambda.serialize path lam

let dump_groups diagnostics groups =
  let path =
    next_path diagnostics ~kind:"lam" ~pass:"groups" ~extension:".lambda"
  in
  Ext_log.dwarn ~__POS__ "Dumping groups to %s" path;
  Ext_fmt.with_file_as_pp path (fun fmt ->
      Format.pp_print_list ~pp_sep:Format.pp_print_newline Lam_group.pp_group
        fmt groups)

let dump_js diagnostics ~pass program =
  let path = next_path diagnostics ~kind:"js" ~pass ~extension:".jsx" in
  Ext_log.dwarn ~__POS__ "Dumping JS pass %s to %s" pass path;
  Ext_pervasives.with_file_as_chan path (fun channel ->
      Js_dump_program.dump_program program channel)
