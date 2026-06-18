open Lsp
open Types

module Uri_map = Map.Make (Uri)

type diagnostics = Diagnostic.t list Uri_map.t

type t = {diagnostics: diagnostics; send: PublishDiagnosticsParams.t -> unit}

let empty () = Uri_map.empty

let create ~diagnostics ~send = {diagnostics; send}

let from_uri ~uri (d : Diagnostic.t list) = Uri_map.add uri d (empty ())

(* Compiler log diagnostics are a full snapshot of the latest build output.
   Overwrite the previous compiler log diagnostics so files that disappeared
   from the new log receive an empty diagnostic list and stale errors are
   cleared in the client. *)
let overwrite ~(new_diagnostics : diagnostics) t =
  let diagnostics =
    Uri_map.merge
      (fun _ existing incoming ->
        match (existing, incoming) with
        | None, None -> Some []
        | Some _, None -> Some []
        | None, Some diagnostics -> Some diagnostics
        | Some _, Some diagnostics -> Some diagnostics)
      t.diagnostics new_diagnostics
  in
  {t with diagnostics}

let append ~(new_diagnostics : diagnostics) t =
  let diagnostics =
    Uri_map.merge
      (fun _ existing incoming ->
        match (existing, incoming) with
        | None, None -> Some []
        | Some diagnostics, None -> Some diagnostics
        | None, Some diagnostics -> Some diagnostics
        | Some a, Some b -> Some (a @ b))
      t.diagnostics new_diagnostics
  in
  {t with diagnostics}

let send t =
  Uri_map.iter
    (fun uri diagnostics ->
      t.send (PublishDiagnosticsParams.create ~uri ~diagnostics ()))
    t.diagnostics

(* Convert parsed compiler-log entries into LSP diagnostics grouped by document URI.
   Compiler logs may report paths either relative to the workspace root or as
   absolute paths, so this function normalizes each entry into a DocumentUri
   before publishing.
   Syntax errors are skipped here because they are produced from the current
   in-memory document text during TextDocumentDidChange notification.
   Compiler log diagnostics are used for build output such as type errors,
   warnings, and circular dependencies. *)
let to_lsp_format (workspace_root : DocumentUri.t)
    (doc_store : Document_store.t) diagnostics =
  let workspace_root_path = workspace_root |> DocumentUri.to_path in

  let expand_range (uri : Uri.t) (range : Range.t) =
    let shortest_possible_code = "let a=1" in

    match Document_store.get_opt ~uri doc_store with
    | None ->
      let is_zero =
        range.start.line = 0 && range.start.line = 0 && range.end_.line = 0
        && range.end_.character = 0
      in
      if is_zero then
        {
          range with
          end_ =
            {line = 0; character = String.length shortest_possible_code - 1};
        }
      else range
    | Some {text} ->
      (* TODO: Revisit this *)
      let lines = String.split_on_char '\n' text in

      let line, character =
        match List.rev lines with
        | [] -> (0, String.length shortest_possible_code - 1)
        | last_line :: rest ->
          let line_count = List.length rest in
          (line_count - 1, String.length last_line - 1)
      in
      {range with end_ = {line; character}}
  in

  let diagnostics_sanitized =
    diagnostics
    |> List.filter_map
         (fun
           (filepath, (diagnostic_entry : Compiler_log.Parse.diagnostic_entry))
         ->
           let uri =
             match filepath with
             | Compiler_log.Parse.Relative_path p ->
               DocumentUri.of_path (Filename.concat workspace_root_path p)
             | Full_path p -> DocumentUri.of_path p
           in

           match diagnostic_entry.entry with
           | Syntax_error -> None
           | Circular_dependency ->
             (* Circular dependency diagnostics are special-cased because the compiler log
                does not point at a precise source range. When the document is open, we expand
                the diagnostic range to cover the whole document so the editor can display a
                file-level diagnostic.*)
             let diagnostic =
               {
                 diagnostic_entry.diagnostic with
                 range = expand_range uri diagnostic_entry.diagnostic.range;
               }
             in
             Some (uri, diagnostic)
           | Warning {number; configured_as_error} ->
             let warning_msg = "Warning " ^ string_of_int number in
             let message =
               match diagnostic_entry.diagnostic.message with
               | `String m -> m
               | `MarkupContent {value} -> value
             in
             let severity =
               match diagnostic_entry.diagnostic.severity with
               | Some DiagnosticSeverity.Warning when configured_as_error ->
                 Some DiagnosticSeverity.Error
               | other -> other
             in
             Some
               ( uri,
                 {
                   diagnostic_entry.diagnostic with
                   message = `String (message ^ warning_msg);
                   range = expand_range uri diagnostic_entry.diagnostic.range;
                   severity;
                 } )
           | Common_error | Unknow ->
             Some
               ( uri,
                 {
                   diagnostic_entry.diagnostic with
                   range = expand_range uri diagnostic_entry.diagnostic.range;
                 } ))
  in

  diagnostics_sanitized
  |> List.fold_left
       (fun acc (uri, diagnostic) ->
         Uri_map.update uri
           (function
             | None -> Some [diagnostic]
             | Some diagnostics -> Some (diagnostic :: diagnostics))
           acc)
       Uri_map.empty

let collect_diagnostics_from_log_using_source_dirs workspace_root fs =
  let ( /+ ) = Filename.concat in
  let workspace_root_path = workspace_root |> Lsp.Types.DocumentUri.to_path in
  let path =
    workspace_root_path /+ Constants.compiler_dir_partial_path
    /+ Constants.sources_dirs
  in
  let build_roots = Source_dirs.get_build_roots_from_file ~fs path in
  let diagnostics =
    match build_roots with
    | Some build_roots ->
      build_roots
      |> List.filter_map (fun build_root ->
             let compiler_log_path =
               workspace_root_path /+ build_root /+ Constants.compiler_log
             in
             match Fs.load ~fs compiler_log_path with
             | Some content ->
               Some (Compiler_log.Parse.parse_log_content content)
             | None -> None)
      |> List.flatten
    | None -> []
  in
  diagnostics
