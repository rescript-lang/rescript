let source_is_newer ~source ~artifact =
  match
    (File_util.modification_time source, File_util.modification_time artifact)
  with
  | Some source_time, Some artifact_time -> source_time > artifact_time
  | Some _, None -> true
  | None, _ -> false

let source_is_not_older_than_ast compile_assets ~root ~source_mtimes path =
  let absolute = Filename.concat root path in
  match Hashtbl.find_opt source_mtimes path with
  | None ->
    source_is_newer ~source:absolute
      ~artifact:
        (Build_artifacts.published_ast_path
           ~ocaml_dir:(Build_artifacts.lib_path root "ocaml")
           path)
  | Some source_modified -> (
    match Compile_assets.ast compile_assets absolute with
    | None -> true
    | Some ast -> source_modified >= ast.modified)
