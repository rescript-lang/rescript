open Lsp
open Lsp.Types

(* Compiler paths and client URIs may differ in drive-letter case. Compare
   their wire forms, as Document_store does, without lowercasing file names. *)
module Uri_map = Map.Make (struct
  type t = Uri.t

  let compare left right =
    String.compare (Uri.to_string left) (Uri.to_string right)
end)

type diagnostics = Diagnostic.t list Uri_map.t

(* Diagnostics come from sources with different lifetimes:
   - compiler diagnostics are workspace/build state;
   - compiler syntax diagnostics are only trusted after a document is closed;
   - syntax diagnostics for open documents must follow the unsaved editor buffer.
   Keep them separate so one source can be refreshed or cleared without
   accidentally dropping diagnostics owned by another source. *)
type t = {
  (* Non-syntax build diagnostics parsed from .compiler.log, such as type
     errors, warnings, and dependency cycles. *)
  compiler: diagnostics;
  (* Syntax diagnostics parsed from .compiler.log. These are only published for
     closed documents because open documents use the editor buffer below. *)
  compiler_syntax: diagnostics;
  (* Syntax diagnostics produced from currently open in-memory documents. *)
  syntax: diagnostics;
  send: PublishDiagnosticsParams.t -> unit;
}

let empty () = Uri_map.empty

let make ~diagnostics ~send =
  {compiler = diagnostics; compiler_syntax = empty (); syntax = empty (); send}

let merge_diagnostics left right =
  Uri_map.merge
    (fun _ left right ->
      match (left, right) with
      | None, None -> None
      | Some diagnostics, None | None, Some diagnostics -> Some diagnostics
      | Some left, Some right -> Some (left @ right))
    left right

let get_all t : diagnostics =
  let compiler = merge_diagnostics t.compiler t.compiler_syntax in
  merge_diagnostics t.syntax compiler

let diagnostics ?(include_syntax = true) t =
  if include_syntax then get_all t
  else merge_diagnostics t.compiler t.compiler_syntax

let replace_snapshot ~old ~latest =
  Uri_map.merge
    (fun _ existing incoming ->
      match (existing, incoming) with
      | None, None -> None
      | Some _, None -> Some []
      | None, Some diagnostics | Some _, Some diagnostics -> Some diagnostics)
    old latest

let update_compiler ~(new_diagnostics : diagnostics) t =
  {t with compiler = replace_snapshot ~old:t.compiler ~latest:new_diagnostics}

let update_compiler_syntax ~(new_diagnostics : diagnostics) t =
  {
    t with
    compiler_syntax =
      replace_snapshot ~old:t.compiler_syntax ~latest:new_diagnostics;
  }

let update_syntax ~uri ~(new_diagnostics : Diagnostic.t list) t =
  {t with syntax = Uri_map.add uri new_diagnostics t.syntax}

let clear_syntax ~uri t = update_syntax ~uri ~new_diagnostics:[] t

let send ?(include_syntax = true) ?(force_publish_uris = [])
    ?(should_publish = fun _ -> true) t =
  let diagnostics = diagnostics ~include_syntax t in
  Uri_map.iter
    (fun uri diagnostics ->
      if should_publish uri then
        t.send (PublishDiagnosticsParams.create ~uri ~diagnostics ()))
    diagnostics;
  force_publish_uris
  |> List.iter (fun uri ->
      (* A URI excluded from this push may still have diagnostics retained
            from an earlier push. Publish an empty list when ownership changes
            so the client does not combine that stale result with pull data. *)
      if (not (should_publish uri)) || not (Uri_map.mem uri diagnostics) then
        t.send (PublishDiagnosticsParams.create ~uri ~diagnostics:[] ()))

(* Convert parsed compiler-log entries into LSP diagnostics grouped by document
   URI. Compiler logs may report paths either relative to the workspace root or
   as absolute paths, so this function normalizes each entry into a DocumentUri
   before publishing.

   Open-document syntax diagnostics come from the current editor buffer. Compiler
   syntax diagnostics are only useful for closed documents, where the compiler
   log is the freshest source we have. *)
let to_lsp_format ~include_syntax ~include_non_syntax
    (workspace_root : DocumentUri.t) (doc_store : Document_store.t) diagnostics
    =
  let workspace_root_path = workspace_root |> DocumentUri.to_path in

  let range_of_entry uri = function
    | Some range -> range
    | None -> (
      match Document_store.get_opt ~uri doc_store with
      | None ->
        let shortest_possible_code = "let a=1" in
        Document.range_of_text shortest_possible_code
      | Some doc -> Document.range_of_text (Document.text doc))
  in

  let diagnostic_of_entry uri (entry : Compiler_log.Parse.diagnostic_entry) =
    let message, severity =
      match entry.kind with
      | Warning {number; configured_as_error} ->
        let default = "Warning " ^ string_of_int number in
        if configured_as_error then
          ( default ^ " (configured as error) - " ^ entry.message,
            DiagnosticSeverity.Error )
        else (default ^ " - " ^ entry.message, DiagnosticSeverity.Warning)
      | Unknown -> ("Unknown error - " ^ entry.message, DiagnosticSeverity.Error)
      | _ -> (entry.message, DiagnosticSeverity.Error)
    in
    Diagnostic.create ~severity ~source:"ReScript"
      ~range:(range_of_entry uri entry.range)
      ~message:(`String message) ()
  in

  let diagnostics_sanitized =
    diagnostics
    |> List.filter_map (fun (entry : Compiler_log.Parse.diagnostic_entry) ->
        let uri =
          match entry.path with
          | Compiler_log.Parse.Relative_path p ->
            Uri.of_path (Filename.concat workspace_root_path p)
          | Full_path p -> Uri.of_path p
        in

        match entry.kind with
        | Syntax_error ->
          if
            include_syntax
            && Option.is_none (Document_store.get_opt ~uri doc_store)
          then Some (uri, diagnostic_of_entry uri entry)
          else None
        | Circular_dependency ->
          if not include_non_syntax then None
          else Some (uri, diagnostic_of_entry uri entry)
        | Warning _ | Common_error | Unknown ->
          if not include_non_syntax then None
          else Some (uri, diagnostic_of_entry uri entry))
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

let has_non_syntax_diagnostics diagnostics =
  diagnostics
  |> List.exists
       (fun (diagnostic_entry : Compiler_log.Parse.diagnostic_entry) ->
         match diagnostic_entry.kind with
         | Syntax_error -> false
         | Warning _ | Circular_dependency | Common_error | Unknown -> true)

let update_from_compiler_log ~workspace_root ~doc_store compiler_log t =
  let compiler_syntax =
    compiler_log
    |> to_lsp_format ~include_syntax:true ~include_non_syntax:false
         workspace_root doc_store
  in
  let t = update_compiler_syntax ~new_diagnostics:compiler_syntax t in
  (* A syntax-only compiler log is an incomplete build snapshot: the compiler
     stopped before type checking the rest of the workspace. Keep the previous
     non-syntax compiler diagnostics until the next complete build snapshot or
     successful empty log arrives. *)
  if compiler_log = [] || has_non_syntax_diagnostics compiler_log then
    let compiler =
      compiler_log
      |> to_lsp_format ~include_syntax:false ~include_non_syntax:true
           workspace_root doc_store
    in
    update_compiler ~new_diagnostics:compiler t
  else t

let diagnostics_from_completed_logs contents =
  if List.for_all Compiler_log.Parse.is_complete contents then
    contents
    |> List.map Compiler_log.Parse.parse_log_content
    |> List.flatten |> Option.some
  else None

let collect_diagnostics_from_log_using_source_dirs workspace_root fs =
  let ( /+ ) = Filename.concat in
  let workspace_root_path = workspace_root |> Uri.to_path in
  let path =
    workspace_root_path /+ Constants.compiler_dir_partial_path
    /+ Constants.sources_dirs
  in
  (* .sourcedirs.json is the source of truth for compiler-log locations. It is
     written by the build system and includes every build root that can emit a
     .compiler.log. Fall back to the root log only when that metadata is missing
     or empty, so opening a document can still publish the last known diagnostics
     from older or partial builds. *)
  let build_roots =
    Source_dirs.get_build_roots_from_file ~fs path |> Option.value ~default:[]
  in
  build_roots
  |> List.filter_map (fun build_root ->
      let compiler_log_path =
        workspace_root_path /+ build_root /+ Constants.compiler_log
      in
      Fs.load ~fs compiler_log_path)
  |> diagnostics_from_completed_logs
