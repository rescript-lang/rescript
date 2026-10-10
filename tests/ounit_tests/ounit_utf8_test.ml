(* https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
*)

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [
         ( "escape malformed UTF-8 in JavaScript strings" >:: fun _ ->
           Js_dump_string.escape_to_string
             "\xc0\x80\xed\xa0\x80\xf4\x90\x80\x80"
           =~ {|"\xc0\x80\xed\xa0\x80\xf4\x90\x80\x80"|} );
         ( __LOC__ >:: fun _ ->
           Code_frame.break_long_line 4 "abc—def" =~ ["abc—"; "def"] );
       ]
