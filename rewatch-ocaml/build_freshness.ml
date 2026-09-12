let source_requires_parse source_modified artifact_modified =
  match (source_modified, artifact_modified) with
  | Some source_time, Some artifact_time -> source_time >= artifact_time
  | Some _, None -> true
  | None, _ -> false

let source_is_not_older_than_ast compile_assets ~root ~source_mtimes path =
  let absolute = Filename.concat root path in
  let source_modified =
    match Hashtbl.find_opt source_mtimes path with
    | Some modified -> Some modified
    | None -> File_util.modification_time absolute
  in
  let artifact_modified =
    match Compile_assets.ast compile_assets absolute with
    | Some ast -> Some ast.modified
    | None ->
      let published_ast =
        Build_artifacts.published_ast_path
          ~ocaml_dir:(Build_artifacts.lib_path root "ocaml")
          path
      in
      let belongs_to_source =
        try
          (Ast_header.read published_ast).source
          |> Option.exists (fun source ->
              Platform.normalize_path_for_comparison source
              = Platform.normalize_path_for_comparison absolute)
        with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
      in
      if belongs_to_source then File_util.modification_time published_ast
      else None
  in
  source_requires_parse source_modified artifact_modified
