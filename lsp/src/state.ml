open Lsp.Types

type status =
  | Uninitialized
  | Initialized of {
      params: InitializeParams.t;
      diagnostics: Diagnostics.t;
      analysis_state: Analysis.Shared_types.state;
      compiler_config: Compiler_config.t;
    }

type t = {
  status: status;
  store: Document_store.t;
  configuration: Configuration.t;
  fs: Eio.Fs.dir_ty Eio.Path.t;
}

let create ~store ~configuration ~fs =
  {status = Uninitialized; store; configuration; fs}

let initialize t ~params ~diagnostics ~analysis_state ~compiler_config =
  {
    t with
    status = Initialized {params; diagnostics; analysis_state; compiler_config};
  }

let diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init -> init.diagnostics

let params t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init -> init.params

(* TODO: rewrite this? *)
let update_diagnostics diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {params; analysis_state; compiler_config} ->
    {
      t with
      status =
        Initialized {params; analysis_state; diagnostics; compiler_config};
    }

let analysis_state t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {analysis_state} -> analysis_state

let compiler_config t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {compiler_config} -> compiler_config

let workspace_root t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init ->
    Helpers.workspace_root_uri_of_initialize_params init.params

let to_yojson (t : t) : Yojson.Safe.t =
  let document_store_to_yojson (store : Document_store.t) =
    store.documents |> Hashtbl.to_seq
    |> Seq.map (fun (uri, {Document_store.text; version}) ->
           ( Lsp.Uri.to_string uri,
             `Assoc
               [
                 ("version", `Int version);
                 ("text_length", `Int (String.length text));
               ] ))
    |> List.of_seq
    |> fun fields -> `Assoc fields
  in

  let diagnostics_to_yojson (diagnostics : Diagnostics.t) =
    diagnostics |> Diagnostics.diagnostics |> Diagnostics.Uri_map.to_seq
    |> Seq.map (fun (uri, diagnostics) ->
           ( Lsp.Uri.to_string uri,
             `List (List.map Diagnostic.yojson_of_t diagnostics) ))
    |> List.of_seq
    |> fun fields -> `Assoc fields
  in

  let compiler_config_to_yojson (compiler_config : Compiler_config.t) =
    compiler_config |> Compiler_config.Uri_map.to_seq
    |> Seq.map (fun (uri, config) ->
           (Lsp.Uri.to_string uri, Compiler_config.Parse.to_yojson config))
    |> List.of_seq
    |> fun fields -> `Assoc fields
  in

  let status_to_yojson = function
    | Uninitialized -> `Assoc [("kind", `String "Uninitialized")]
    | Initialized {params; diagnostics; compiler_config} ->
      `Assoc
        [
          ("kind", `String "Initialized");
          ("diagnostics", diagnostics_to_yojson diagnostics);
          ("compiler_config", compiler_config_to_yojson compiler_config);
          ("params", InitializeParams.yojson_of_t params);
        ]
  in

  `Assoc
    [
      ("status", status_to_yojson t.status);
      ("store", document_store_to_yojson t.store);
      ("configuration", Configuration.to_yojson t.configuration);
      ( "analysis_state",
        Analysis.Shared_types.state_to_yojson (analysis_state t) );
    ]
