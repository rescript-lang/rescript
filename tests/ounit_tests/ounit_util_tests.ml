let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           let buf = Ext_buffer.create 0 in
           Ext_buffer.add_string_char buf "hello" 'v';
           Ext_buffer.contents buf =~ "hellov";
           Ext_buffer.length buf =~ 6 );
         ( __LOC__ >:: fun _ ->
           String.length (Digest.to_hex (Digest.string "")) =~ 32 );
       ]
