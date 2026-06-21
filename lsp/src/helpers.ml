open Lsp
open Lsp.Types

let workspace_root_uri_of_initialize_params (params : InitializeParams.t) =
  match (params.workspaceFolders, params.rootUri, params.rootPath) with
  | Some (Some workspace_folders), Some root_uri, None ->
    let root =
      match workspace_folders with
      | [] -> root_uri
      | ws :: _ -> ws.uri
    in
    root
  | _, Some root_uri, _ -> root_uri
  | _, _, Some (Some root_path) -> root_path |> Uri.of_path
  | _ ->
    failwith
      ("Failed to find workspace root uri of initialize params. Received: "
      ^ Yojson.Safe.pretty_to_string (InitializeParams.yojson_of_t params))

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

let relative_to ~root path =
  let root = if String.ends_with ~suffix:"/" root then root else root ^ "/" in

  if String.starts_with ~prefix:root path then
    let root_len = String.length root in
    String.sub path root_len (String.length path - root_len)
  else path

let to_camel_case (text : string) : string =
  let len = String.length text in
  let buffer = Buffer.create len in

  let is_separator = function
    | ' ' | '\t' | '\n' | '\r' | '-' -> true
    | _ -> false
  in

  let rec loop i capitalize_next =
    if i >= len then Buffer.contents buffer
    else
      let c = text.[i] in

      if is_separator c then loop (i + 1) true
      else if capitalize_next then (
        Buffer.add_char buffer (Char.uppercase_ascii c);
        loop (i + 1) false)
      else (
        Buffer.add_char buffer c;
        loop (i + 1) false)
  in

  loop 0 true

let get_cmi_file ~(uri : Uri.t) ~(compiler_config : Compiler_config.t)
    ~(fs : Fs.fs) ~workspace_root =
  let config_roots =
    compiler_config |> Compiler_config.Uri_map.to_seq
    |> Seq.map (fun (uri, config) -> (uri |> Uri.to_path, config))
    |> List.of_seq |> List.map fst
  in

  let config =
    match
      config_roots |> best_root_match ~path:(workspace_root |> Uri.to_path)
    with
    | Some root ->
      Compiler_config.Uri_map.find_opt (Uri.of_path root) compiler_config
    | None -> None
  in
  match config with
  | Some config ->
    let namespace =
      match config.namespace with
      | Some (Namespace_bool true) -> config.name |> to_camel_case
      | Some (Namespace_string name) -> name |> to_camel_case
      | _ -> ""
    in
    let suffix_to_append =
      if String.length namespace > 0 then "-" ^ namespace else ""
    in
    let path = Uri.to_path uri in
    let resi_file =
      match best_root_match ~path config_roots with
      | Some package_root_path ->
        let cmi_file =
          let open Filename in
          let ( /+ ) = concat in
          let relative_path = relative_to ~root:package_root_path path in
          let sources_dir = dirname relative_path in
          let cmi =
            let res_file = basename relative_path ^ suffix_to_append in
            remove_extension res_file ^ ".cmi"
          in
          package_root_path /+ "lib" /+ "bs" /+ sources_dir /+ cmi
        in
        let result =
          match Fs.exists ~follow:false ~fs cmi_file with
          | true -> Some cmi_file
          | false -> None
        in
        result
      | None -> None
    in
    resi_file
  | None -> None

let get_compiled_file ~(uri : Uri.t) ~(compiler_config : Compiler_config.t)
    ~(fs : Fs.fs) ~workspace_root =
  let config_roots =
    compiler_config |> Compiler_config.Uri_map.to_seq
    |> Seq.map (fun (uri, config) -> (uri |> Uri.to_path, config))
    |> List.of_seq |> List.map fst
  in

  let config =
    match
      config_roots |> best_root_match ~path:(workspace_root |> Uri.to_path)
    with
    | Some root ->
      Compiler_config.Uri_map.find_opt (Uri.of_path root) compiler_config
    | None -> None
  in
  match config with
  | Some config ->
    let {Compiler_config.suffix; folder; in_source} =
      Compiler_config.get_output_config config
    in

    let js_file_path =
      let path = Uri.to_path uri in

      let file_path =
        let open Filename in
        let ( /+ ) = concat in
        if in_source then
          let filename = basename path in
          let js_file = remove_extension filename ^ suffix in
          Some (dirname path /+ js_file)
        else
          match best_root_match ~path config_roots with
          | Some package_root_path ->
            (*
              Some example with sources
              package_root_path: /home/pedro/Desktop/projects/rescript-lang.org/apps/guide
              path:              /home/pedro/Desktop/projects/rescript-lang.org/apps/guide/app/GuideHome.res
              compiled js:       /home/pedro/Desktop/projects/rescript-lang.org/apps/guide/lib/es6/app/GuideHome.jsx
            *)
            (* app/GuideHome.res *)
            let relative_path = relative_to ~root:package_root_path path in
            let sources_dir = dirname relative_path in
            let js_file =
              let res_file = basename relative_path in
              remove_extension res_file ^ suffix
            in
            Some (package_root_path /+ "lib" /+ folder /+ sources_dir /+ js_file)
          | None -> None
      in
      match file_path with
      | Some file_path when Fs.exists ~fs ~follow:false file_path ->
        Some (file_path |> Uri.of_path)
      | _ -> None
    in

    js_file_path
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
