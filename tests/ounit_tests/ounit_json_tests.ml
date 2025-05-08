let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "escape 'hello'" >:: fun _ ->
           let escaped = Json.escape "hello" in
           let expected = "hello" in
           OUnit.assert_equal escaped expected );
         ( "escape \\x17" >:: fun _ ->
           let escaped = Json.escape "\x17" in
           let expected = "\\u0017" in
           OUnit.assert_equal escaped expected );
       ]
