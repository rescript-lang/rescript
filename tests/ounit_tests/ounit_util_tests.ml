let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           Ext_pervasives.nat_of_string_exn "003" =~ 3;
           (try
              Ext_pervasives.nat_of_string_exn "0a" |> ignore;
              2
            with _ -> -1)
           =~ -1 );
         ( __LOC__ >:: fun _ ->
           let cursor = ref 0 in
           let v = Ext_pervasives.parse_nat_of_string "123a" cursor in
           (v, !cursor) =~ (123, 3);
           cursor := 0;
           let v = Ext_pervasives.parse_nat_of_string "a" cursor in
           (v, !cursor) =~ (0, 0) );
         ( __LOC__ >:: fun _ ->
           let buf = Ext_buffer.create 0 in
           Ext_buffer.add_string_char buf "hello" 'v';
           Ext_buffer.contents buf =~ "hellov";
           Ext_buffer.length buf =~ 6 );
         ( __LOC__ >:: fun _ ->
           let buf = Ext_buffer.create 0 in
           Ext_buffer.add_char_string buf 'h' "ellov";
           Ext_buffer.contents buf =~ "hellov";
           Ext_buffer.length buf =~ 6 );
         ( __LOC__ >:: fun _ ->
           String.length (Digest.to_hex (Digest.string "")) =~ 32 );
       ]
