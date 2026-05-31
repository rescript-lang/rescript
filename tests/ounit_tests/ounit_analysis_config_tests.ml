let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let json raw =
  match Yojson.Safe.from_string raw with
  | json -> json
  | exception Yojson.Json_error message -> failwith message

let assert_namespace ~expected raw =
  OUnit.assert_equal
    ~printer:(function
      | Some s -> "Some " ^ s
      | None -> "None")
    expected
    (Analysis.FindFiles.getNamespace (json raw))

let suites =
  __FILE__
  >::: [
         ( "absent namespace is disabled" >:: fun _ ->
           assert_namespace ~expected:None {|{"name": "@tests/pkg"}|} );
         ( "false namespace is disabled" >:: fun _ ->
           assert_namespace ~expected:None
             {|{"name": "@tests/pkg", "namespace": false}|} );
         ( "non-string non-bool namespace is disabled" >:: fun _ ->
           assert_namespace ~expected:None
             {|{"name": "@tests/pkg", "namespace": 1}|} );
         ( "true namespace uses package name" >:: fun _ ->
           assert_namespace ~expected:(Some "TestsNamespacedReferences")
             {|{"name": "@tests/namespaced-references", "namespace": true}|} );
         ( "string namespace uses explicit value" >:: fun _ ->
           assert_namespace ~expected:(Some "MyNamespace")
             {|{"name": "@tests/pkg", "namespace": "my-namespace"}|} );
       ]
