let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "escape 'hello'" >:: fun _ ->
           let escaped = `String "hello" |> Yojson.Safe.to_string in
           let expected = "\"hello\"" in
           OUnit.assert_equal escaped expected );
         ( "escape \\x17" >:: fun _ ->
           let escaped = `String "\x17" |> Yojson.Safe.to_string in
           let expected = "\"\\u0017\"" in
           OUnit.assert_equal escaped expected );
       ]
