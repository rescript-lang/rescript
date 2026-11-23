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

let%expect_test "uses the build's output defaults when package specs are absent"
    =
  Test.with_project ~name:"output-defaults" ~text:{|let value = 1|}
    ~rescript_json:{|{"name":"output-defaults","sources":{"dir":"."}}|}
    (fun ~root ~doc client -> Test.snapshot ~root client (open_compiled doc));
  [%expect {| { "uri": "file:///workspace/Test.js" } |}]

let%expect_test
    "uses the build's default in-source setting within package specs" =
  Test.with_project ~name:"output-default-in-source" ~text:{|let value = 1|}
    ~rescript_json:
      {|{"name":"output-default-in-source","sources":{"dir":"."},"suffix":".js","package-specs":{"module":"commonjs","suffix":".cjs"}}|}
    (fun ~root ~doc client -> Test.snapshot ~root client (open_compiled doc));
  [%expect {| { "uri": "file:///workspace/Test.cjs" } |}]

let%expect_test "selects the first existing configured output" =
  Test.with_project ~name:"output-multiple" ~text:{|let value = 1|}
    ~rescript_json:
      {|{"name":"output-multiple","sources":{"dir":"."},"package-specs":[{"module":"commonjs","suffix":".cjs","in-source":false},{"module":"esmodule","suffix":".mjs","in-source":false}]}|}
    (fun ~root ~doc client ->
      Test.snapshot ~root client (open_compiled doc);
      Sys.remove (root /+ "lib" /+ "js" /+ "Test.cjs");
      Test.snapshot ~root client (open_compiled doc));
  [%expect
    {|
    { "uri": "file:///workspace/lib/js/Test.cjs" }
    { "uri": "file:///workspace/lib/es6/Test.mjs" }
    |}]

let%expect_test
    "uses the build's scoped-package namespace for interface artifacts" =
  Test.with_project ~name:"namespace-package" ~text:{|let value = 1|}
    ~rescript_json:
      {|{"name":"@example/my-lib","namespace":true,"sources":{"dir":"."}}|}
    (fun ~root ~doc client ->
      assert (
        Sys.file_exists (root /+ "lib" /+ "rescript" /+ "Test-ExampleMyLib.cmi"));
      Create_interface.create_and_print ~root ~doc client);
  [%expect
    {|
    { "uri": "file:///workspace/Test.resi" }
    let value: int
    |}]

let%expect_test "uses the build's namespace-entry artifact names" =
  Test.with_project ~name:"namespace-entry" ~text:{|let value = Other.value|}
    ~files:[("Other.res", {|let value = 1|})]
    ~rescript_json:
      {|{"name":"namespace-entry","namespace":"my_lib","namespace-entry":"Test","sources":{"dir":"."}}|}
    (fun ~root ~doc client ->
      let other =
        Test.make_document ~text:{|let value = 1|}
          ~uri:(Uri.of_path (root /+ "Other.res"))
      in
      assert (Sys.file_exists (root /+ "lib" /+ "rescript" /+ "Other-@MyLib.cmi"));
      Test.perform client (Test.open_document other);
      Create_interface.create_and_print ~root ~doc:other client;
      Create_interface.create_and_print ~root ~doc client);
  [%expect
    {|
    { "uri": "file:///workspace/Other.resi" }
    let value: int
    { "uri": "file:///workspace/Test.resi" }
    let value: int
    |}]

let%expect_test "uses the dependency's module format and suffix for output" =
  with_dependency ~in_source:false (fun ~root ~root_doc ~doc client ->
      Test.snapshot ~root client (open_compiled root_doc);
      Test.snapshot ~root client (open_compiled doc));
  [%expect
    {|
    { "uri": "file:///workspace/lib/js/Main.cjs" }
    {
      "uri": "file:///workspace/node_modules/child-package/lib/es6/src/Child.mjs"
    }
    |}]

let%expect_test "uses the dependency's in-source output setting" =
  with_dependency ~in_source:true (fun ~root ~root_doc:_ ~doc client ->
      Test.snapshot ~root client (open_compiled doc));
  [%expect
    {| { "uri": "file:///workspace/node_modules/child-package/src/Child.mjs" } |}]

let%expect_test
    "creates a nested dependency's interface from a flat namespaced artifact" =
  with_dependency ~in_source:false (fun ~root ~root_doc:_ ~doc client ->
      assert (
        Sys.file_exists
          (root /+ "node_modules" /+ "child-package" /+ "lib" /+ "rescript"
         /+ "Child-ChildPackage.cmi"));
      Create_interface.create_and_print ~root ~doc client);
  [%expect
    {|
    { "uri": "file:///workspace/node_modules/child-package/src/Child.resi" }
    let answer: int
    |}]
