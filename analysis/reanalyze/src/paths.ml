let rescript_json = "rescript.json"

(** If `t` is an object (`Assoc), get the value associated with the given string key *)
let get key t =
  match t with
  | `Assoc items -> List.assoc_opt key items
  | _ -> None

let read_file filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let rec find_project_root ~dir =
  let rescript_json_file = Filename.concat dir rescript_json in
  if Sys.file_exists rescript_json_file then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ rescript_json ^ ".");
      assert false)
    else find_project_root ~dir:parent

let run_config = Run_config.run_config

let set_project_root_from_cwd () =
  run_config.project_root <- find_project_root ~dir:(Sys.getcwd ());
  run_config.bsb_project_root <-
    (match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> run_config.project_root
    | Some s -> s)

let set_rescript_project_root = lazy (set_project_root_from_cwd ())

module Config = struct
  let read_suppress conf =
    match conf |> get "suppress" with
    | Some (`List elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Yojson.Safe.t) ->
               match x with
               | `String s -> Some s
               | _ -> None)
      in
      run_config.suppress <- names @ run_config.suppress
    | _ -> ()

  let read_unsuppress conf =
    match conf |> get "unsuppress" with
    | Some (`List elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Yojson.Safe.t) ->
               match x with
               | `String s -> Some s
               | _ -> None)
      in
      run_config.unsuppress <- names @ run_config.unsuppress
    | _ -> ()

  let read_analysis conf =
    match conf |> get "analysis" with
    | Some (`List elements) ->
      elements
      |> List.iter (fun (x : Yojson.Safe.t) ->
             match x with
             | `String "all" -> Run_config.all ()
             | `String "dce" -> Run_config.dce ()
             | `String "exception" -> Run_config.exception_ ()
             | `String "termination" -> Run_config.termination ()
             | _ -> ())
    | _ ->
      (* if no "analysis" specified, default to dce *)
      Run_config.dce ()

  let read_transitive conf =
    match conf |> get "transitive" with
    | Some (`Bool bool) -> Run_config.transitive bool
    | _ -> ()

  (* Read the config from rescript.json and apply it to runConfig and suppress and unsuppress *)
  let process_config () =
    set_project_root_from_cwd ();
    let rescript_file = Filename.concat run_config.project_root rescript_json in

    let process_text text =
      match try Some (Yojson.Safe.from_string text) with _ -> None with
      | Some json -> (
        match get "reanalyze" json with
        | Some conf ->
          read_suppress conf;
          read_unsuppress conf;
          read_analysis conf;
          read_transitive conf
        | None ->
          (* if no "analysis" specified, default to dce *)
          Run_config.dce ())
      | _ -> ()
    in

    match read_file rescript_file with
    | Some text -> process_text text
    | None -> ()
end

(**
  * Handle namespaces in cmt files.
  * E.g. src/Module-Project.cmt becomes src/Module
  *)
let handle_namespace cmt =
  let cut_after_dash s =
    match String.index s '-' with
    | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
    | exception Not_found -> s
  in
  let no_dir = Filename.basename cmt = cmt in
  if no_dir then cmt |> Filename.remove_extension |> cut_after_dash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> Filename.remove_extension |> cut_after_dash
    in
    Filename.concat dir base

let get_module_name cmt = cmt |> handle_namespace |> Filename.basename

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

let read_cmt_scan () =
  let source_dirs_file =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left Filename.concat run_config.bsb_project_root
  in
  let get_fn key fn json =
    get key json |> Option.to_list |> List.filter_map fn
  in
  let read_entry (json : Yojson.Safe.t) =
    let build_root = json |> get_fn "build_root" Yojson_helpers.string_opt in
    let scan_dirs =
      match json |> get "scan_dirs" with
      | Some (`List arr) -> arr |> List.filter_map Yojson_helpers.string_opt
      | _ -> []
    in
    let also_scan_build_root =
      match json |> get_fn "also_scan_build_root" Yojson_helpers.bool_opt with
      | [b] -> b
      | _ -> false
    in
    match build_root with
    | [build_root] -> Some {build_root; scan_dirs; also_scan_build_root}
    | _ -> None
  in
  match read_file source_dirs_file with
  | None -> []
  | Some text -> (
    match try Some (Yojson.Safe.from_string text) with _ -> None with
    | None -> []
    | Some json -> (
      match get "cmt_scan" json with
      | Some (`List arr) -> arr |> List.filter_map read_entry
      | _ -> []))
