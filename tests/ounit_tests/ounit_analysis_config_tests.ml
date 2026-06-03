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
    (Analysis.Find_files.get_namespace (json raw))

let assert_string_opt ~expected actual =
  OUnit.assert_equal
    ~printer:(function
      | Some s -> "Some " ^ s
      | None -> "None")
    expected actual

let assert_bool_opt ~expected actual =
  OUnit.assert_equal
    ~printer:(function
      | Some b -> "Some " ^ string_of_bool b
      | None -> "None")
    expected actual

let suites =
  __FILE__
  >::: [
         ( "yojson helpers do not raise on type mismatch" >:: fun _ ->
           assert_string_opt ~expected:(Some "value")
             (Analysis.Yojson_helpers.string_opt (`String "value"));
           assert_string_opt ~expected:None
             (Analysis.Yojson_helpers.string_opt (`Bool true));
           assert_bool_opt ~expected:(Some true)
             (Analysis.Yojson_helpers.bool_opt (`Bool true));
           assert_bool_opt ~expected:None
             (Analysis.Yojson_helpers.bool_opt (`String "true"));
           OUnit.assert_equal ~printer:string_of_int 1
             (Analysis.Yojson_helpers.to_list_opt (`List [`Null])
             |> Option.fold ~none:0 ~some:List.length);
           OUnit.assert_equal ~printer:string_of_int 0
             (Analysis.Yojson_helpers.to_list_opt (`String "not a list")
             |> Option.fold ~none:0 ~some:List.length);
           OUnit.assert_bool "valid JSON parses"
             (Analysis.Yojson_helpers.from_string_opt {|{"ok": true}|}
             |> Option.is_some);
           OUnit.assert_bool "invalid JSON is ignored"
             (Analysis.Yojson_helpers.from_string_opt {|{|} |> Option.is_none)
         );
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
