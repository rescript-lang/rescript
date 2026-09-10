open Build_artifacts
open File_util

let source_is_newer ~source ~artifact =
  match modification_time source, modification_time artifact with
  | Some source_time, Some artifact_time -> source_time > artifact_time
  | Some _, None -> true
  | None, _ -> false

let source_is_not_older_than_ast compile_assets ~root ~source_mtimes path =
  let absolute = Filename.concat root path in
  match Hashtbl.find_opt source_mtimes path with
  | None ->
    source_is_newer ~source:absolute
      ~artifact:
        (Filename.concat (lib_path root "ocaml")
           (Filename.basename (Source.ast_path path)))
  | Some source_modified -> (
    match Compile_assets.ast compile_assets absolute with
    | None -> true
    | Some ast -> source_modified >= ast.modified)

let published_ast_path ~ocaml_dir source_path =
  (* bsc gives its intermediate AST an epoch mtime. The copy published after a
     successful parse is the stable freshness marker across build cycles. *)
  Filename.concat ocaml_dir (Filename.basename (Source.ast_path source_path))
