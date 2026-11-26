open Lsp
open Lsp.Types
open Rescript_language_server_core
open Rescript_language_server_core.Diagnostics

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
