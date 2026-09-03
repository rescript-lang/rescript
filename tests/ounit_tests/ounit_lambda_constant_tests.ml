open OUnit

let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [
         ( "typed string constants" >:: fun _ ->
           Lambda.const_string "value" =~ Lambda.Const_string "value" );
         ( "compiler-generated strings normalize malformed bytes" >:: fun _ ->
           let constant = Lambda.const_string "a\xffé" in
           constant =~ Lambda.Const_string "aÿé";
           match
             Lambda.prim ~primitive:Lambda.Pstringlength
               ~args:[Lambda.const constant]
               Location.none
           with
           | Lambda.Lconst (Lambda.Const_int length) -> 3l =~ length
           | _ -> OUnit.assert_failure "expected a folded string length" );
       ]
