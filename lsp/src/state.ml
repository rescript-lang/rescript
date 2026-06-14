open Lsp.Types

type status =
  | Uninitialized
  | Initialized of {
      params: InitializeParams.t;
      diagnostics: Diagnostics.t;
      analysis_state: Analysis.Shared_types.state;
      package: Analysis.Shared_types.package option;
    }

type t = {
  status: status;
  store: Document_store.t;
  configuration: Configuration.t;
  fs: Eio.Fs.dir_ty Eio.Path.t;
}

let create ~store ~configuration ~fs =
  {status = Uninitialized; store; configuration; fs}

let initialize t ~params ~diagnostics ~analysis_state ~package =
  {t with status = Initialized {params; diagnostics; analysis_state; package}}

let diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init -> init.diagnostics

(* NOTE: rewrite this? *)
let update_diagnostics diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {params; analysis_state; package} ->
    {t with status = Initialized {params; analysis_state; package; diagnostics}}

let analysis_state t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {analysis_state} -> analysis_state

let package t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {package} -> package

let update_package package t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {params; diagnostics; analysis_state} ->
    {t with status = Initialized {params; analysis_state; package; diagnostics}}

let workspace_root t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init ->
    Helpers.workspace_root_uri_of_initialize_params init.params

let to_yojson (t : t) : Yojson.Safe.t =
  let minimal = true in
  let document_store_to_yojson (store : Document_store.t) =
    store.documents |> Hashtbl.to_seq
    |> Seq.map (fun (uri, {Document_store.text; version}) ->
           ( Lsp.Uri.to_string uri,
             `Assoc
               ([
                  ("version", `Int version);
                  ("text_length", `Int (String.length text));
                ]
               @ if not minimal then [("text", `String text)] else []) ))
    |> List.of_seq
    |> fun fields -> `Assoc fields
  in

  let diagnostics_to_yojson (diagnostics : Diagnostics.t) =
    diagnostics.diagnostics |> Diagnostics.Uri_map.to_seq
    |> Seq.map (fun (uri, diagnostics) ->
           ( Lsp.Uri.to_string uri,
             `List (List.map Diagnostic.yojson_of_t diagnostics) ))
    |> List.of_seq
    |> fun fields -> `Assoc fields
  in

  let status_to_yojson = function
    | Uninitialized -> `Assoc [("kind", `String "Uninitialized")]
    | Initialized {params; diagnostics} ->
      `Assoc
        ([
           ("kind", `String "Initialized");
           ("diagnostics", diagnostics_to_yojson diagnostics);
         ]
        @
        if not minimal then [("params", InitializeParams.yojson_of_t params)]
        else [])
  in

  `Assoc
    [
      ("status", status_to_yojson t.status);
      ("store", document_store_to_yojson t.store);
      ( "analysis_state",
        Analysis.Shared_types.state_to_yojson (analysis_state t) );
    ]
