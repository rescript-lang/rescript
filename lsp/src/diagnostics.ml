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

let%expect_test "get_all merges diagnostics from every source" =
  let uri = Uri.of_path "/workspace/A.res" in
  let range =
    Range.create
      ~start:(Position.create ~line:0 ~character:0)
      ~end_:(Position.create ~line:0 ~character:1)
  in
  let diagnostic message =
    Diagnostic.create ~range ~message:(`String message) ()
  in
  let t =
    {
      compiler = Uri_map.singleton uri [diagnostic "compiler"];
      compiler_syntax = Uri_map.singleton uri [diagnostic "compiler syntax"];
      syntax = Uri_map.singleton uri [diagnostic "syntax"];
      send = (fun _ -> ());
    }
  in
  get_all t |> Uri_map.find uri
  |> List.iter (fun (diagnostic : Diagnostic.t) ->
      match diagnostic.message with
      | `String message -> print_endline message
      | `MarkupContent {value} -> print_endline value);
  [%expect {|
    syntax
    compiler
    compiler syntax |}]

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

let%expect_test "send can reserve an open document for pull diagnostics" =
  let open_uri = Uri.of_path "/workspace/Open.res" in
  let closed_uri = Uri.of_path "/workspace/Closed.res" in
  let range =
    Range.create
      ~start:(Position.create ~line:0 ~character:0)
      ~end_:(Position.create ~line:0 ~character:1)
  in
  let diagnostic message =
    Diagnostic.create ~range ~message:(`String message) ()
  in
  let print ({uri; diagnostics; _} : PublishDiagnosticsParams.t) =
    Printf.printf "%s: %d\n"
      (Filename.basename (Uri.to_path uri))
      (List.length diagnostics)
  in
  let t =
    {
      compiler =
        Uri_map.empty
        |> Uri_map.add open_uri [diagnostic "open compiler"]
        |> Uri_map.add closed_uri [diagnostic "closed compiler"];
      compiler_syntax = Uri_map.empty;
      syntax = Uri_map.singleton open_uri [diagnostic "open syntax"];
      send = print;
    }
  in
  send ~include_syntax:false ~force_publish_uris:[open_uri]
    ~should_publish:(fun uri -> Uri.compare uri open_uri <> 0)
    t;
  [%expect {|
    Closed.res: 1
    Open.res: 0 |}]

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

let%expect_test "only completed compiler logs form a diagnostics snapshot" =
  let print = function
    | None -> print_endline "incomplete"
    | Some diagnostics ->
      Printf.printf "complete: %d diagnostics\n" (List.length diagnostics)
  in
  diagnostics_from_completed_logs ["#Start(1)\n"] |> print;
  [%expect {| incomplete |}];

  diagnostics_from_completed_logs ["#Start(1)\n#Done(2)\n"] |> print;
  [%expect {| complete: 0 diagnostics |}];

  diagnostics_from_completed_logs ["#Start(1)\n#Done(2)\n"; "#Start(3)\n"]
  |> print;
  [%expect {| incomplete |}]

let%expect_test "compiler syntax diagnostics don't clear type diagnostics" =
  let workspace_root = Uri.of_path "/workspace" in
  let uri_a = Uri.of_path "/workspace/A.res" in
  let uri_b = Uri.of_path "/workspace/B.res" in
  let uri_c = Uri.of_path "/workspace/C.res" in
  let range =
    Range.create
      ~start:(Position.create ~line:0 ~character:0)
      ~end_:(Position.create ~line:0 ~character:1)
  in
  let diagnostic message =
    Diagnostic.create ~range ~message:(`String message) ()
  in
  let entry uri kind message =
    let open Compiler_log.Parse in
    {kind; path = Full_path (Uri.to_path uri); message; range = Some range}
  in
  let print t =
    diagnostics t
    |> Uri_map.iter (fun uri diagnostics ->
        let messages =
          diagnostics
          |> List.map (fun (diagnostic : Diagnostic.t) ->
              match diagnostic.message with
              | `String message -> message
              | `MarkupContent {value} -> value)
        in
        Printf.printf "%s: %s\n"
          (Filename.basename (Uri.to_path uri))
          (match messages with
          | [] -> "<empty>"
          | messages -> String.concat ", " messages))
  in
  let doc_store = Document_store.make () in
  let t = make ~diagnostics:(empty ()) ~send:(fun _ -> ()) in
  let t =
    update_from_compiler_log ~workspace_root ~doc_store
      [entry uri_b Common_error "type B"; entry uri_c Common_error "type C"]
      t
  in
  print t;
  [%expect {|
    B.res: type B
    C.res: type C |}];

  let doc =
    Document.make
      (DidOpenTextDocumentParams.create
         ~textDocument:
           (TextDocumentItem.create ~languageId:(LanguageKind.Other "rescript")
              ~uri:uri_a ~text:"let x =" ~version:1))
      ~position_encoding:`UTF16
  in
  ignore (Document_store.add doc_store ~doc);
  let t = update_syntax ~uri:uri_a ~new_diagnostics:[diagnostic "syntax A"] t in
  let t =
    update_from_compiler_log ~workspace_root ~doc_store
      [entry uri_a Syntax_error "compiler syntax A"]
      t
  in
  print t;
  [%expect {|
    A.res: syntax A
    B.res: type B
    C.res: type C |}];

  ignore (Document_store.remove doc_store ~uri:uri_a);
  let t = clear_syntax ~uri:uri_a t in
  let t =
    update_from_compiler_log ~workspace_root ~doc_store
      [entry uri_a Syntax_error "compiler syntax A"]
      t
  in
  print t;
  [%expect
    {|
    A.res: compiler syntax A
    B.res: type B
    C.res: type C |}];

  let t =
    update_from_compiler_log ~workspace_root ~doc_store
      [entry uri_b Common_error "type B fixed later"]
      t
  in
  print t;
  [%expect
    {|
    A.res: <empty>
    B.res: type B fixed later
    C.res: <empty> |}];

  let t = update_from_compiler_log ~workspace_root ~doc_store [] t in
  print t;
  [%expect {|
    A.res: <empty>
    B.res: <empty>
    C.res: <empty> |}]

let%expect_test "Windows compiler paths share the open document's identity" =
  (* Exercise real backslash paths even when this test runs on Unix. *)
  let win32 = !Uri.Private.win32 in
  Fun.protect
    ~finally:(fun () -> Uri.Private.win32 := win32)
    (fun () ->
      Uri.Private.win32 := true;
      let workspace_root = Uri.of_path {|D:\a\project|} in
      let compiler_uri = Uri.of_path {|D:\a\project\src\Broken.res|} in
      let client_uri = Uri.of_string "file:///d%3A/a/project/src/Broken.res" in
      assert (Uri.compare compiler_uri client_uri <> 0);
      let source = "let value = 1\nlet other = 2" in
      let doc =
        Document.make
          (DidOpenTextDocumentParams.create
             ~textDocument:
               (TextDocumentItem.create ~uri:client_uri ~text:source ~version:0
                  ~languageId:(LanguageKind.Other "rescript")))
          ~position_encoding:`UTF16
      in
      let doc_store = Document_store.make () in
      ignore (Document_store.add doc_store ~doc);
      let entry path kind =
        {Compiler_log.Parse.kind; path; message = "compiler"; range = None}
      in
      let convert path kind =
        to_lsp_format ~include_syntax:true ~include_non_syntax:true
          workspace_root doc_store
          [entry path kind]
      in
      let absolute =
        Compiler_log.Parse.Full_path {|D:\a\project\src\Broken.res|}
      in
      let relative = Compiler_log.Parse.Relative_path {|src\Broken.res|} in
      let compiler = convert absolute Common_error in
      let relative_compiler = convert relative Common_error in
      Printf.printf "absolute compiler diagnostics: %d\n"
        (List.length (Uri_map.find client_uri compiler));
      Printf.printf "relative compiler diagnostics: %d\n"
        (List.length (Uri_map.find client_uri relative_compiler));
      Printf.printf "open compiler syntax diagnostics: %d\n"
        (Uri_map.cardinal (convert absolute Syntax_error));
      let compiler_diagnostic = List.hd (Uri_map.find client_uri compiler) in
      Printf.printf "fallback range uses open buffer: %b\n"
        (compiler_diagnostic.range = Document.range_of_text source);
      let t =
        make ~diagnostics:compiler
          ~send:(fun ({diagnostics} : PublishDiagnosticsParams.t) ->
            Printf.printf "pushed diagnostics: %d\n" (List.length diagnostics))
        |> update_syntax ~uri:client_uri
             ~new_diagnostics:
               [
                 Diagnostic.create
                   ~range:(Document.range_of_text source)
                   ~message:(`String "buffer") ();
               ]
      in
      Printf.printf "merged URI keys: %d\n" (Uri_map.cardinal (get_all t));
      Printf.printf "merged diagnostics: %d\n"
        (List.length (Uri_map.find client_uri (get_all t)));
      send ~include_syntax:false ~force_publish_uris:[client_uri]
        ~should_publish:(fun uri ->
          Document_store.get_opt doc_store ~uri = None)
        t;
      ignore (Document_store.remove doc_store ~uri:compiler_uri);
      let t = clear_syntax ~uri:compiler_uri t in
      send t;
      send (update_compiler ~new_diagnostics:(empty ()) t);
      Printf.printf "filename case remains distinct: %b\n"
        (Uri_map.cardinal
           (Uri_map.empty |> Uri_map.add client_uri []
           |> Uri_map.add
                (Uri.of_string "file:///d%3A/a/project/src/broken.res")
                [])
        = 2));
  [%expect
    {|
    absolute compiler diagnostics: 1
    relative compiler diagnostics: 1
    open compiler syntax diagnostics: 0
    fallback range uses open buffer: true
    merged URI keys: 1
    merged diagnostics: 2
    pushed diagnostics: 0
    pushed diagnostics: 1
    pushed diagnostics: 0
    filename case remains distinct: true
    |}]
