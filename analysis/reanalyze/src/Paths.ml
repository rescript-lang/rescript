module StringMap = Map_string

let rescriptJson = "rescript.json"

let readFile filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let rec findProjectRoot ~dir =
  let rescriptJsonFile = Filename.concat dir rescriptJson in
  if Sys.file_exists rescriptJsonFile then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ rescriptJson ^ ".");
      assert false)
    else findProjectRoot ~dir:parent

let runConfig = RunConfig.runConfig

let setProjectRootFromCwd () =
  runConfig.projectRoot <- findProjectRoot ~dir:(Sys.getcwd ());
  runConfig.bsbProjectRoot <-
    (match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> runConfig.projectRoot
    | Some s -> s)

let setReScriptProjectRoot = lazy (setProjectRootFromCwd ())

module Config = struct
  let readSuppress conf =
    match Json.get "suppress" conf with
    | Some (Array elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Json.t) ->
               match x with
               | String s -> Some s
               | _ -> None)
      in
      runConfig.suppress <- names @ runConfig.suppress
    | _ -> ()

  let readUnsuppress conf =
    match Json.get "unsuppress" conf with
    | Some (Array elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Json.t) ->
               match x with
               | String s -> Some s
               | _ -> None)
      in
      runConfig.unsuppress <- names @ runConfig.unsuppress
    | _ -> ()

  let readAnalysis conf =
    match Json.get "analysis" conf with
    | Some (Array elements) ->
      elements
      |> List.iter (fun (x : Json.t) ->
             match x with
             | String "all" -> RunConfig.all ()
             | String "dce" -> RunConfig.dce ()
             | String "exception" -> RunConfig.exception_ ()
             | String "termination" -> RunConfig.termination ()
             | _ -> ())
    | _ ->
      (* if no "analysis" specified, default to dce *)
      RunConfig.dce ()

  let readTransitive conf =
    match Json.get "transitive" conf with
    | Some True -> RunConfig.transitive true
    | Some False -> RunConfig.transitive false
    | _ -> ()

  (* Read the config from rescript.json and apply it to runConfig and suppress and unsuppress *)
  let processBsconfig () =
    setProjectRootFromCwd ();
    let rescriptFile = Filename.concat runConfig.projectRoot rescriptJson in

    let processText text =
      match Json.parse text with
      | None -> ()
      | Some json -> (
        match Json.get "reanalyze" json with
        | Some conf ->
          readSuppress conf;
          readUnsuppress conf;
          readAnalysis conf;
          readTransitive conf
        | None ->
          (* if no "analysis" specified, default to dce *)
          RunConfig.dce ())
    in

    match readFile rescriptFile with
    | Some text -> processText text
    | None -> ()
end

(**
  * Handle namespaces in cmt files.
  * E.g. src/Module-Project.cmt becomes src/Module
  *)
let handleNamespace cmt =
  let cutAfterDash s =
    match String.index s '-' with
    | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
    | exception Not_found -> s
  in
  let noDir = Filename.basename cmt = cmt in
  if noDir then cmt |> Filename.remove_extension |> cutAfterDash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> Filename.remove_extension |> cutAfterDash
    in
    Filename.concat dir base

let getModuleName cmt = cmt |> handleNamespace |> Filename.basename

let readDirsFromConfig ~configSources =
  let dirs = ref [] in
  let root = runConfig.projectRoot in
  let rec processDir ~subdirs dir =
    let absDir =
      match dir = "" with
      | true -> root
      | false -> Filename.concat root dir
    in
    if Sys.file_exists absDir && Sys.is_directory absDir then (
      dirs := dir :: !dirs;
      if subdirs then
        absDir |> Sys.readdir
        |> Array.iter (fun d -> processDir ~subdirs (Filename.concat dir d)))
  in
  let rec processSourceItem (sourceItem : Ext_json_types.t) =
    match sourceItem with
    | Str {str} -> str |> processDir ~subdirs:false
    | Obj {map} -> (
      match StringMap.find_opt map "dir" with
      | Some (Str {str}) ->
        let subdirs =
          match StringMap.find_opt map "subdirs" with
          | Some (True _) -> true
          | Some (False _) -> false
          | _ -> false
        in
        str |> processDir ~subdirs
      | _ -> ())
    | Arr {content = arr} -> arr |> Array.iter processSourceItem
    | _ -> ()
  in
  (match configSources with
  | Some sourceItem -> processSourceItem sourceItem
  | None -> ());
  !dirs

let readSourceDirs ~configSources =
  let sourceDirs =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left Filename.concat runConfig.bsbProjectRoot
  in
  let dirs = ref [] in
  let readDirs json =
    match json with
    | Ext_json_types.Obj {map} -> (
      match StringMap.find_opt map "dirs" with
      | Some (Arr {content = arr}) ->
        arr
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str {str} -> dirs := str :: !dirs
               | _ -> ());
        ()
      | _ -> ())
    | _ -> ()
  in
  if sourceDirs |> Sys.file_exists then
    let jsonOpt = sourceDirs |> Ext_json_parse.parse_json_from_file in
    match jsonOpt with
    | exception _ -> ()
    | json ->
      if runConfig.bsbProjectRoot <> runConfig.projectRoot then (
        readDirs json;
        dirs := readDirsFromConfig ~configSources)
      else readDirs json
  else (
    if !Cli.debug then (
      Log_.item "Warning: can't find source dirs: %s\n" sourceDirs;
      Log_.item "Types for cross-references will not be found.\n");
    dirs := readDirsFromConfig ~configSources);
  !dirs

type cmt_scan_entry = {
  build_root: string;
  scan_dirs: string list;
  also_scan_build_root: bool;
}
(** Read explicit `.cmt/.cmti` scan plan from `.sourcedirs.json`.

    This is a v2 extension produced by `rewatch` to support monorepos without requiring
    reanalyze-side package resolution.

    The scan plan is a list of build roots (usually `<pkg>/lib/bs`) relative to the project root,
    plus a list of subdirectories (relative to that build root) to scan for `.cmt/.cmti`.

    If missing, returns the empty list and callers should fall back to legacy behavior. *)

let readCmtScan () =
  let sourceDirsFile =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left Filename.concat runConfig.bsbProjectRoot
  in
  let entries = ref [] in
  let read_entry (json : Ext_json_types.t) =
    match json with
    | Ext_json_types.Obj {map} -> (
      let build_root =
        match StringMap.find_opt map "build_root" with
        | Some (Ext_json_types.Str {str}) -> Some str
        | _ -> None
      in
      let scan_dirs =
        match StringMap.find_opt map "scan_dirs" with
        | Some (Ext_json_types.Arr {content = arr}) ->
          arr |> Array.to_list
          |> List.filter_map (fun x ->
                 match x with
                 | Ext_json_types.Str {str} -> Some str
                 | _ -> None)
        | _ -> []
      in
      let also_scan_build_root =
        match StringMap.find_opt map "also_scan_build_root" with
        | Some (Ext_json_types.True _) -> true
        | Some (Ext_json_types.False _) -> false
        | _ -> false
      in
      match build_root with
      | Some build_root ->
        entries := {build_root; scan_dirs; also_scan_build_root} :: !entries
      | None -> ())
    | _ -> ()
  in
  let read_cmt_scan (json : Ext_json_types.t) =
    match json with
    | Ext_json_types.Obj {map} -> (
      match StringMap.find_opt map "cmt_scan" with
      | Some (Ext_json_types.Arr {content = arr}) ->
        arr |> Array.iter read_entry
      | _ -> ())
    | _ -> ()
  in
  if sourceDirsFile |> Sys.file_exists then (
    let jsonOpt = sourceDirsFile |> Ext_json_parse.parse_json_from_file in
    match jsonOpt with
    | exception _ -> []
    | json ->
      read_cmt_scan json;
      !entries |> List.rev)
  else []
