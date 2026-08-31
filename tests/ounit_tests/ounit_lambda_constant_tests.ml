open OUnit

let ( =~ ) = OUnit.assert_equal

let assert_string_constant raw_delim expected_delim =
  Lambda.const_string "value" raw_delim
  =~ Lambda.Const_string {s = "value"; delim = expected_delim}

let suites =
  __FILE__
  >::: [
         ( "processed string delimiters" >:: fun _ ->
           assert_string_constant None (Some DNone);
           assert_string_constant (Some "json") (Some DNoQuotes);
           assert_string_constant (Some "bq") (Some DBackQuotes);
           assert_string_constant (Some "js") None );
       ]
