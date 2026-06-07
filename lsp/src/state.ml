open Lsp.Types

type status =
  | Uninitialized
  | Initialized of {params: InitializeParams.t; diagnostics: Diagnostics.t}

type t = {
  status: status;
  store: Document_store.t;
  fs: Eio.Fs.dir_ty Eio.Path.t;
  analysis_state: Analysis.Shared_types.state;
}

let create ~store ~fs =
  {
    status = Uninitialized;
    store;
    fs;
    analysis_state = Analysis.Shared_types.create_state ();
  }

let initialize t ~params ~diagnostics =
  {t with status = Initialized {params; diagnostics}}

let diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init -> init.diagnostics

(* NOTE: rewrite this? *)
let update_diagnostics diagnostics t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized {params; _} ->
    {t with status = Initialized {params; diagnostics}}

let workspace_root t =
  match t.status with
  | Uninitialized -> assert false
  | Initialized init -> (
    match init.params.rootUri with
    | None -> assert false
    | Some uri -> uri)
