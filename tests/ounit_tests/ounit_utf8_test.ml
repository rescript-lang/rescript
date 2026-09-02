(* https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
*)

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           Ext_utf8.decode_utf8_string "hello 你好，中华民族 hei"
           =~ [
                104;
                101;
                108;
                108;
                111;
                32;
                20320;
                22909;
                65292;
                20013;
                21326;
                27665;
                26063;
                32;
                104;
                101;
                105;
              ] );
         (__LOC__ >:: fun _ -> Ext_utf8.decode_utf8_string "" =~ []);
         ( "reject malformed UTF-8" >:: fun _ ->
           List.iter
             (fun input ->
               OUnit.assert_raises
                 (Ext_utf8.Invalid_utf8 "Invalid UTF-8 sequence") (fun () ->
                   ignore (Ext_utf8.decode_utf8_string input)))
             ["\xc0\x80"; "\xed\xa0\x80"; "\xf4\x90\x80\x80"] );
         ( "escape malformed UTF-8 in JavaScript strings" >:: fun _ ->
           Js_dump_string.escape_to_string
             "\xc0\x80\xed\xa0\x80\xf4\x90\x80\x80"
           =~ {|"\xc0\x80\xed\xa0\x80\xf4\x90\x80\x80"|} );
         ( __LOC__ >:: fun _ ->
           Code_frame.break_long_line 4 "abc—def" =~ ["abc—"; "def"] );
       ]
