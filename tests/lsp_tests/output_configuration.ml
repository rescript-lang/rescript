open Lsp
open Types

let ( /+ ) = Filename.concat

let with_dependency ~in_source f =
  let root = Filename.temp_file "rescript-lsp-output-config-" "" in
  Sys.remove root;
  Test.mkdir_p root;
  Fun.protect
    ~finally:(fun () -> Test.remove_project root)
    (fun () ->
      let dependency = root /+ "node_modules" /+ "child-package" in
      let config =
        `Assoc
          [
            ("name", `String "child-package");
            ("namespace", `String "ChildPackage");
            ("sources", `Assoc [("dir", `String "src")]);
            ( "package-specs",
              `Assoc
                [("module", `String "esmodule"); ("in-source", `Bool in_source)]
            );
            ("suffix", `String ".mjs");
          ]
        |> Yojson.Safe.to_string
      in
      Test.write_file (dependency /+ "rescript.json") config;
      Test.write_file
        (dependency /+ "package.json")
        {|{"name":"child-package","version":"1.0.0"}|};
      let doc =
        Test.make_document
          ~uri:(Uri.of_path (dependency /+ "src" /+ "Child.res"))
          ~text:{|let answer = 42
|}
      in
      Test.write_file (Uri.to_path doc.uri) doc.text;
      let root_doc =
        Test.make_document
          ~uri:(Uri.of_path (root /+ "Main.res"))
          ~text:{|let answer = 1
|}
      in
      Test.setup_project ~root ~run_build:true ~documents:[root_doc]
        ~rescript_json:
          {|{
  "name": "root-package",
  "namespace": "RootPackage",
  "dependencies": ["child-package"],
  "sources": {"dir": "."},
  "package-specs": {"module": "commonjs", "in-source": false},
  "suffix": ".cjs"
}|};
      (* Workspace builds apply the root's output settings to dependencies.
         Model an independently produced dependency output, retaining the real
         build metadata above for discovery and interface creation. *)
      let compiled =
        if in_source then dependency /+ "src" /+ "Child.mjs"
        else dependency /+ "lib" /+ "es6" /+ "src" /+ "Child.mjs"
      in
      Test.write_file compiled {|export const answer = 42;
|};
      Test.with_client ~root (fun client ->
          Test.perform client (Test.open_document doc);
          f ~root ~root_doc ~doc client))

let open_compiled doc =
  Client_request.UnknownRequest
    {
      meth = "textDocument/openCompiled";
      params =
        Some
          (Jsonrpc.Structured.t_of_yojson
             (TextDocumentIdentifier.yojson_of_t (Test.text_document doc)));
    }

let%expect_test "uses the dependency's module format and suffix for output" =
  with_dependency ~in_source:false (fun ~root ~root_doc ~doc client ->
      Test.snapshot ~root client (open_compiled root_doc);
      Test.snapshot ~root client (open_compiled doc));
  [%expect
    {|
    { "uri": "file:///workspace/lib/js/./Main.cjs" }
    {
      "uri": "file:///workspace/node_modules/child-package/lib/es6/src/Child.mjs"
    }
    |}]

let%expect_test "uses the dependency's in-source output setting" =
  with_dependency ~in_source:true (fun ~root ~root_doc:_ ~doc client ->
      Test.snapshot ~root client (open_compiled doc));
  [%expect
    {| { "uri": "file:///workspace/node_modules/child-package/src/Child.mjs" } |}]

let%expect_test "uses the dependency's namespace when creating an interface" =
  with_dependency ~in_source:false (fun ~root ~root_doc:_ ~doc client ->
      Create_interface.create_and_print ~root ~doc client);
  [%expect
    {|
    { "uri": "file:///workspace/node_modules/child-package/src/Child.resi" }
    let answer: int
    |}]
