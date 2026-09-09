let for_package ~is_local (config : Config.t) =
  if is_local then config.diagnostics
  else
    let report_suffix =
      Package_metadata.issue_tracker_url config.root
      |> Option.map (fun url ->
           "\nPlease report this to the package maintainer: " ^ url)
      |> Option.value ~default:""
    in
    List.map
      (fun diagnostic -> diagnostic ^ report_suffix)
      config.deprecation_diagnostics

let report_missing_source_folder (config : Config.t) path =
  let prefix = Filename.concat config.root "" in
  let relative =
    if String.starts_with ~prefix path then
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    else path
  in
  Printf.eprintf
    "ERROR:\nCould not read folder: %S. Specified in dependency: %s, located %S...\n%!"
    relative config.name config.root

let report_missing_sources ~is_root (config : Config.t) =
  if (not is_root) && not config.sources_defined then
    Printf.eprintf
      "WARN:\nPackage '%s' has not defined any sources, but is not the root package. This is likely a mistake. It is located: %s\n%!"
      config.name config.root

let validate_metadata (config : Config.t) =
  match Package_metadata.package_name config.root with
  | Error message ->
    raise
      (Project_context.Error ("Could not initialize build: " ^ message))
  | Ok (Some package_name) when package_name <> config.name ->
    Printf.eprintf
      "WARN:\n\nPackage name mismatch for %s:\nThe package.json name is %S, while the rescript.json name is %S\nThis inconsistency will cause issues with package resolution.\n\n%!"
      config.root package_name config.name
  | Ok (Some _) | Ok None -> ()
