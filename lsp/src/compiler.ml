module Uri_map = Map.Make (Lsp.Uri)

let collect_diagnostics_from_log_file path =
  let content = Eio.Path.load path in
  Compiler_log.Parse.parse_log_content content

let collect_diagnostics_from_log_using_source_dirs workspace_root
    (state : State.t) =
  let ( // ) = Filename.concat in
  let workspace_root_path = workspace_root |> Lsp.Types.DocumentUri.to_path in
  let path =
    workspace_root_path // Constants.compiler_dir_partial_path
    // Constants.sources_dirs
  in
  let build_roots =
    Source_dirs.get_build_roots_from_file Eio.Path.(state.fs / path)
  in
  let diagnostics =
    match build_roots with
    | Some build_roots ->
      build_roots
      |> List.map (fun build_root ->
             let compiler_log_path =
               workspace_root_path // build_root // Constants.compiler_log
             in
             collect_diagnostics_from_log_file
               Eio.Path.(state.fs / compiler_log_path))
      |> List.flatten
    | None -> []
  in
  diagnostics
