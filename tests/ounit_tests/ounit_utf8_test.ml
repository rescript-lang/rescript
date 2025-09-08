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
       ]
