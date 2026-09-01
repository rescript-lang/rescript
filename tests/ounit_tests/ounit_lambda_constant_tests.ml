open OUnit

let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [
         ( "typed string constants" >:: fun _ ->
           Lambda.const_string "value" =~ Lambda.Const_string "value" );
       ]
