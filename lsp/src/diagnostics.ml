module Uri_map = Map.Make (Lsp.Uri)

type t = {
  diagnostics: Lsp.Types.Diagnostic.t list Uri_map.t;
  send: Lsp.Types.PublishDiagnosticsParams.t list -> unit;
}

let create ~diagnostics ~send = {diagnostics; send}

let set ~(diagnostics : Lsp.Types.Diagnostic.t list Uri_map.t) t =
  let diagnostics =
    Uri_map.merge
      (fun _ existing incoming ->
        match (existing, incoming) with
        | None, None -> Some []
        | Some _, None -> Some []
        | None, Some diagnostics -> Some diagnostics
        | Some _, Some diagnostics -> Some diagnostics)
      t.diagnostics diagnostics
  in
  {t with diagnostics}

let send t =
  Uri_map.iter
    (fun uri diagnostics ->
      t.send [Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics ()])
    t.diagnostics

let convert_to_lsp (workspace_root : Lsp.Types.DocumentUri.t) diagnostics =
  let workspace_root_path = workspace_root |> Lsp.Types.DocumentUri.to_path in
  diagnostics
  |> List.fold_left
       (fun acc
            (filepath, (diagnostic_entry : Compiler_log.Parse.diagnostic_entry))
          ->
         let document_uri =
           match filepath with
           | Compiler_log.Parse.Relative_path p ->
             Lsp.Types.DocumentUri.of_path
               (Filename.concat workspace_root_path p)
           | Full_path p -> Lsp.Types.DocumentUri.of_path p
         in

         Uri_map.update document_uri
           (function
             | None -> Some [diagnostic_entry.diagnostic]
             | Some diagnostics ->
               Some (diagnostic_entry.diagnostic :: diagnostics))
           acc)
       Uri_map.empty
