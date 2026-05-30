let rescriptJson = "rescript.json"

(** If `t` is an object (`Assoc), get the value associated with the given string key *)
let get key t =
  match t with
  | `Assoc items -> List.assoc_opt key items
  | _ -> None

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
    match conf |> get "suppress" with
    | Some (`List elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Yojson.Safe.t) ->
               match x with
               | `String s -> Some s
               | _ -> None)
      in
      runConfig.suppress <- names @ runConfig.suppress
    | _ -> ()

  let readUnsuppress conf =
    match conf |> get "unsuppress" with
    | Some (`List elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Yojson.Safe.t) ->
               match x with
               | `String s -> Some s
               | _ -> None)
      in
      runConfig.unsuppress <- names @ runConfig.unsuppress
    | _ -> ()

  let readAnalysis conf =
    match conf |> get "analysis" with
    | Some (`List elements) ->
      elements
      |> List.iter (fun (x : Yojson.Safe.t) ->
             match x with
             | `String "all" -> RunConfig.all ()
             | `String "dce" -> RunConfig.dce ()
             | `String "exception" -> RunConfig.exception_ ()
             | `String "termination" -> RunConfig.termination ()
             | _ -> ())
    | _ ->
      (* if no "analysis" specified, default to dce *)
      RunConfig.dce ()

  let readTransitive conf =
    match conf |> get "transitive" with
    | Some (`Bool bool) -> RunConfig.transitive bool
    | _ -> ()

  (* Read the config from rescript.json and apply it to runConfig and suppress and unsuppress *)
  let processConfig () =
    setProjectRootFromCwd ();
    let rescriptFile = Filename.concat runConfig.projectRoot rescriptJson in

    let processText text =
      match try Some (Yojson.Safe.from_string text) with _ -> None with
      | Some json -> (
        match get "reanalyze" json with
        | Some conf ->
          readSuppress conf;
          readUnsuppress conf;
          readAnalysis conf;
          readTransitive conf
        | None ->
          (* if no "analysis" specified, default to dce *)
          RunConfig.dce ())
      | _ -> ()
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
  let get_fn key fn json =
    get key json |> Option.to_list |> List.filter_map fn
  in
  let read_entry (json : Yojson.Safe.t) =
    let build_root =
      json |> get_fn "build_root" Yojson.Safe.Util.to_string_option
    in
    let scan_dirs =
      match json |> get "scan_dirs" with
      | Some (`List arr) ->
        arr |> List.filter_map Yojson.Safe.Util.to_string_option
      | _ -> []
    in
    let also_scan_build_root =
      match
        json |> get_fn "also_scan_build_root" Yojson.Safe.Util.to_bool_option
      with
      | [b] -> b
      | _ -> false
    in
    match build_root with
    | [build_root] -> Some {build_root; scan_dirs; also_scan_build_root}
    | _ -> None
  in
  match readFile sourceDirsFile with
  | None -> []
  | Some text -> (
    match try Some (Yojson.Safe.from_string text) with _ -> None with
    | None -> []
    | Some json -> (
      match get "cmt_scan" json with
      | Some (`List arr) -> arr |> List.filter_map read_entry
      | _ -> []))
