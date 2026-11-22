let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           String.length (Digest.to_hex (Digest.string "")) =~ 32 );
       ]
