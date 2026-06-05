open Lsp.Types

type status =
  | Uninitialized
  | Initialized of {params: InitializeParams.t; diagnostics: Diagnostics.t}

(* TODO: add trace, configuration *)
type t = {status: status; store: Document_store.t; env: Eio_unix.Stdenv.base}

let create ~store ~env = {status = Uninitialized; store; env}

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
