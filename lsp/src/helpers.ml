open Lsp
open Lsp.Types

let root_uri_of_initialize_params (params : InitializeParams.t) =
  match (params.workspaceFolders, params.rootUri, params.rootPath) with
  | _, Some root_uri, _ -> root_uri
  | Some (Some (workspace_folder :: _)), _, _ -> workspace_folder.uri
  | _, _, Some (Some root_path) -> root_path |> Uri.of_path
  | _ -> Sys.getcwd () |> Uri.of_path

(* Return the most specific root from [entries] that contains [path].

  Matching is done on path boundaries, so [/repo/packages/app] matches
  [/repo/packages/app/src/A.res], but it does not match
  [/repo/packages/application/src/A.res].

  If multiple roots match, the longest root wins. This matters in monorepos
  where a file can be under both the workspace root and a nested package root.

  Examples:
  - [best_root_match ~path:"/repo/packages/app/src/A.res"
        ["/repo"; "/repo/packages/app"]]
    returns [Some "/repo/packages/app"].
  - [best_root_match ~path:"/repo/packages/application/src/A.res"
        ["/repo/packages/app"]]
    returns [None]. *)
let best_root_match ~path entries =
  let path_matches_root ~path ~root =
    let is_sep = function
      | '/' | '\\' -> true
      | _ -> false
    in
    let root_len = String.length root in
    let path_len = String.length path in
    root_len > 0
    && (path = root
       || path_len > root_len
          && String.starts_with ~prefix:root path
          && (is_sep root.[root_len - 1] || is_sep path.[root_len]))
  in
  entries
  |> List.fold_left
       (fun best root ->
         if path_matches_root ~path ~root then
           match best with
           | None -> Some root
           | Some best_root ->
             if String.length root > String.length best_root then Some root
             else best
         else best)
       None

let config_for_uri ~(uri : Uri.t) (compiler_config : Compiler_config.t) =
  let path = Uri.to_path uri in
  let config_roots =
    compiler_config |> Compiler_config.Uri_map.to_seq
    |> Seq.map (fun (uri, _) -> Uri.to_path uri)
    |> List.of_seq
  in
  match best_root_match ~path config_roots with
  | Some root ->
    Compiler_config.Uri_map.find_opt (Uri.of_path root) compiler_config
  | None -> None

let get_cmi_file ~uri ~compiler_config ~fs =
  match config_for_uri ~uri compiler_config with
  | Some config ->
    let open Rewatch_ocaml_lib in
    let cmi_file =
      Filename.concat
        (Build_artifacts.artifact_dir config.root)
        (Source.compiler_asset_basename config (Uri.to_path uri) ^ ".cmi")
    in
    if Fs.exists ~follow:false ~fs cmi_file then Some cmi_file else None
  | None -> None

let get_compiled_file ~uri ~compiler_config ~fs =
  match config_for_uri ~uri compiler_config with
  | Some config ->
    let open Rewatch_ocaml_lib in
    let relative_path =
      Project_context.relative_to config.root (Uri.to_path uri)
    in
    config.package_specs
    |> List.find_map (fun spec ->
        let path =
          Build_artifacts.generated_js_path config relative_path spec
        in
        if Fs.exists ~follow:false ~fs path then Some (Uri.of_path path)
        else None)
  | None -> None

let load_full uri (analysis_state : Analysis.Shared_types.state) =
  let package_for_path ~path =
    let roots =
      analysis_state.packages_by_root |> Hashtbl.to_seq_keys |> List.of_seq
    in
    match best_root_match ~path roots with
    | Some root -> Hashtbl.find_opt analysis_state.packages_by_root root
    | None -> None
  in

  let path = uri |> Uri.to_path in
  match package_for_path ~path with
  | Some package -> (
    let module_name =
      Analysis.Build_system.namespaced_name package.namespace
        (Analysis.Find_files.get_name path)
    in
    match Analysis.Cmt.full_for_incremental_cmt ~package ~module_name ~uri with
    | Some cmt_info -> Some cmt_info
    | None -> (
      match Hashtbl.find_opt package.paths_for_module module_name with
      | Some paths ->
        let cmt = Analysis.Shared_types.get_cmt_path ~uri paths in
        Analysis.Cmt.full_for_cmt ~module_name ~package ~uri cmt
      | None -> None))
  | None -> None
