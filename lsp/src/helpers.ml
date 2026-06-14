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

(** Return the most specific root from [entries] that contains [path].

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
