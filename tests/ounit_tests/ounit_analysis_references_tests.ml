open OUnit

let show_reference (module_name, path) =
  Printf.sprintf "%s:%s" module_name (String.concat "." path)

let assert_ref_key_eq ~expected ~actual =
  assert_equal ~printer:show_reference expected actual

let suites =
  __FILE__
  >::: [
         ( "without namespace, external reference is unchanged" >:: fun _ ->
           assert_ref_key_eq ~expected:("MyModule2", ["myFunc2"])
             ~actual:
               (Analysis.References.normalize_external_reference_key
                  ~namespace:None ~module_name:"MyModule2" ~path:["myFunc2"]) );
         ( "with namespace, hidden module resolves to public namespace path"
         >:: fun _ ->
           assert_ref_key_eq
             ~expected:("MyNamespace", ["MyModule2"; "myFunc2"])
             ~actual:
               (Analysis.References.normalize_external_reference_key
                  ~namespace:(Some "MyNamespace")
                  ~module_name:"MyModule2-MyNamespace" ~path:["myFunc2"]) );
       ]
