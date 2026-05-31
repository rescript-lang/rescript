open Lsp.Types

type status =
  | Uninitialized
  | Initialized of {params: InitializeParams.t; diagnostics: Diagnostics.t}

(* TODO: add trace, configuration *)
type t = {status: status; store: Document_store.t}

let create ~store = {status = Uninitialized; store}

let initialize t ~params ~diagnostics =
  {t with status = Initialized {params; diagnostics}}
