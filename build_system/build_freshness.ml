(* An AST dated back to time 1 (Build_artifacts.invalidate_ast) is parsed
   again even when its source is dated earlier still, as sources from archives
   built with SOURCE_DATE_EPOCH=0 are. *)
let source_requires_parse source_modified artifact_modified =
  match (source_modified, artifact_modified) with
  | Some source_time, Some artifact_time ->
    artifact_time <= 1. || source_time >= artifact_time
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
      let ast =
        Build_artifacts.ast_path
          ~artifact_dir:(Build_artifacts.artifact_dir root)
          path
      in
      let belongs_to_source =
        try
          match (Ast_header.read ast).source with
          | Some source ->
            Platform.normalize_path_for_comparison source
            = Platform.normalize_path_for_comparison absolute
          | None -> false
        with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
      in
      if belongs_to_source then File_util.modification_time ast else None
  in
  source_requires_parse source_modified artifact_modified
