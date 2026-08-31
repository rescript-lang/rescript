let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let assert_runtime_value ?(delim = Some "*j") ~encoded ~expected () =
  OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected
    (String_literal.runtime_value encoded delim)

let assert_same_runtime_value left right =
  OUnit.assert_equal 0 (String_literal.compare left right)

let suites =
  __FILE__
  >::: [
         ( "plain text" >:: fun _ ->
           assert_runtime_value ~encoded:"plain" ~expected:"plain" () );
         ( "named escapes" >:: fun _ ->
           assert_runtime_value ~encoded:{|\b\f\n\r\t\v\0|}
             ~expected:"\b\012\n\r\t\011\000" () );
         ( "escaped punctuation and non-escapes" >:: fun _ ->
           assert_runtime_value ~encoded:{|\\\"\'\ \$\`\a|}
             ~expected:{|\"' $`a|} () );
         ( "hex escapes" >:: fun _ ->
           assert_runtime_value ~encoded:{|\x61\xE9|} ~expected:"aé" () );
         ( "unicode escapes" >:: fun _ ->
           assert_runtime_value ~encoded:{|\u0061\u20AC|} ~expected:"a€" ();
           assert_runtime_value ~encoded:{|\u{1f600}|} ~expected:"😀" ();
           assert_runtime_value ~encoded:{|\uD83D\uDE00|} ~expected:"😀" () );
         ( "line continuations" >:: fun _ ->
           assert_runtime_value ~encoded:"a\\\nb" ~expected:"ab" ();
           assert_runtime_value ~encoded:"a\\\rb" ~expected:"ab" ();
           assert_runtime_value ~encoded:"a\\\r\nb" ~expected:"ab" () );
         ( "processed literal delimiters" >:: fun _ ->
           assert_runtime_value ~delim:(Some "*j") ~encoded:{|\x61|}
             ~expected:"a" ();
           assert_runtime_value ~delim:(Some "bq") ~encoded:{|\x61|}
             ~expected:"a" () );
         ( "semantic and unprocessed literals remain unchanged" >:: fun _ ->
           assert_runtime_value ~delim:None ~encoded:{|\x61|} ~expected:{|\x61|}
             ();
           assert_runtime_value ~delim:(Some "json") ~encoded:{|\x61|}
             ~expected:{|\x61|} ();
           assert_runtime_value ~delim:(Some "unknown") ~encoded:{|\x61|}
             ~expected:{|\x61|} () );
         ( "invalid encoded values remain unchanged" >:: fun _ ->
           List.iter
             (fun encoded -> assert_runtime_value ~encoded ~expected:encoded ())
             [
               {|trailing\|};
               {|\x6|};
               {|\xGG|};
               {|\u061|};
               {|\u{}|};
               {|\u{110000}|};
               {|\uD800|};
               {|\uDC00|};
               {|\uD800\u0041|};
               {|\uDC00\uD800|};
             ] );
         ( "comparison uses runtime values" >:: fun _ ->
           assert_same_runtime_value ("a", Some "*j") ({|\x61|}, Some "*j");
           assert_same_runtime_value ("😀", None) ({|\u{1f600}|}, Some "*j");
           assert_same_runtime_value
             ({|\uD83D\uDE00|}, Some "*j")
             ({|\u{1f600}|}, Some "*j");
           assert_same_runtime_value ("a\nb", Some "*j") ({|a\x0ab|}, Some "*j");
           OUnit.assert_bool "comparison should use decoded ordering"
             (String_literal.compare ({|\x62|}, Some "*j") ("a", Some "*j") > 0)
         );
         ( "semantic backslashes remain distinct" >:: fun _ ->
           OUnit.assert_bool "semantic backslash must remain distinct"
             (String_literal.compare ({|\x61|}, None) ({|\x61|}, Some "*j") <> 0)
         );
       ]
