let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "code action edits include document version zero" >:: fun _ ->
           let open Lsp.Types in
           let range =
             Range.create
               ~start:(Position.create ~line:1 ~character:2)
               ~end_:(Position.create ~line:1 ~character:3)
           in
           let uri = "file:///workspace/Test.res" in
           let action =
             Analysis.Code_actions.make ~title:"Replace value"
               ~kind:CodeActionKind.QuickFix ~uri ~new_text:"next" ~range
           in
           let open Yojson.Safe.Util in
           let changes =
             CodeAction.yojson_of_t action
             |> member "edit" |> member "documentChanges" |> to_list
           in
           match changes with
           | [change] ->
             let document = member "textDocument" change in
             OUnit.assert_equal (`Int 0) (member "version" document);
             OUnit.assert_equal (`String uri) (member "uri" document);
             OUnit.assert_equal
               (`List
                  [
                    TextEdit.yojson_of_t (TextEdit.create ~range ~newText:"next");
                  ])
               (member "edits" change)
           | _ -> OUnit.assert_failure "expected one document edit" );
         ( "syntax diagnostics identify ReScript in implementations and \
            interfaces"
         >:: fun _ ->
           List.iter
             (fun (kind_file, source) ->
               let diagnostics =
                 Analysis.Diagnostics.document_syntax ~source ~kind_file
               in
               OUnit.assert_bool "invalid source produces diagnostics"
                 (diagnostics <> []);
               List.iter
                 (fun (diagnostic : Lsp.Types.Diagnostic.t) ->
                   OUnit.assert_equal (Some "ReScript") diagnostic.source;
                   OUnit.assert_equal (Some Lsp.Types.DiagnosticSeverity.Error)
                     diagnostic.severity)
                 diagnostics)
             [
               (Analysis.Files.Res, "let ="); (Analysis.Files.Resi, "let value:");
             ] );
         ( "valid source has no syntax diagnostics" >:: fun _ ->
           OUnit.assert_equal []
             (Analysis.Diagnostics.document_syntax ~source:"let value = 1"
                ~kind_file:Analysis.Files.Res) );
         ( "definition links use one-based file positions" >:: fun _ ->
           let link =
             Analysis.Markdown.
               {
                 start_pos = Lsp.Types.Position.create ~line:4 ~character:7;
                 file = "file:///workspace/Test.res";
                 label = "Type definition";
               }
           in
           OUnit.assert_equal ~printer:Fun.id
             "[Type definition](file:///workspace/Test.res#L5,8)"
             (Analysis.Markdown.make_goto_command link) );
         ( "definition links preserve encoded file URIs at the first position"
         >:: fun _ ->
           let link =
             Analysis.Markdown.
               {
                 start_pos = Lsp.Types.Position.create ~line:0 ~character:0;
                 file = "file:///workspace/My%20File%23.res";
                 label = "Type definition";
               }
           in
           OUnit.assert_equal ~printer:Fun.id
             "[Type definition](file:///workspace/My%20File%23.res#L1,1)"
             (Analysis.Markdown.make_goto_command link) );
       ]
