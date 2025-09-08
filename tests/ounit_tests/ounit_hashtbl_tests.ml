let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump

let suites =
  __FILE__
  >::: [
         (* __LOC__ >:: begin fun _ ->  *)
         (*   let h = Hash_string.create 0 in  *)
         (*   let accu key = *)
         (*     Hash_string.replace_or_init h key   succ 1 in  *)
         (*   let count = 1000 in  *)
         (*   for i = 0 to count - 1 do      *)
         (*     Array.iter accu  [|"a";"b";"c";"d";"e";"f"|]     *)
         (*   done; *)
         (*   Hash_string.length h =~ 6; *)
         (*   Hash_string.iter (fun _ v -> v =~ count ) h *)
         (* end; *)
         ( "add semantics " >:: fun _ ->
           let h = Hash_string.create 0 in
           let count = 1000 in
           for _ = 0 to 1 do
             for i = 0 to count - 1 do
               Hash_string.add h (string_of_int i) i
             done
           done;
           Hash_string.length h =~ 2 * count );
         ( "replace semantics" >:: fun _ ->
           let h = Hash_string.create 0 in
           let count = 1000 in
           for _ = 0 to 1 do
             for i = 0 to count - 1 do
               Hash_string.replace h (string_of_int i) i
             done
           done;
           Hash_string.length h =~ count );
         ( __LOC__ >:: fun _ ->
           let h = Hash_string.create 0 in
           let count = 10 in
           for i = 0 to count - 1 do
             Hash_string.replace h (string_of_int i) i
           done;
           let xs = Hash_string.to_list h (fun k _ -> k) in
           let ys = List.sort compare xs in
           ys =~ ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"] );
       ]
