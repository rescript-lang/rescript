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
      ("Failed to find a root path. Initialize params received: "
      ^ Yojson.Safe.pretty_to_string (InitializeParams.yojson_of_t params))
