let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           let set = Hash_set_ident_mask.create 0 in
           let a, b, _, _ =
             ( Ident.create "a",
               Ident.create "b",
               Ident.create "c",
               Ident.create "d" )
           in
           Hash_set_ident_mask.add_unmask set a;
           Hash_set_ident_mask.add_unmask set a;
           Hash_set_ident_mask.add_unmask set b;
           OUnit.assert_bool __LOC__
             (not @@ Hash_set_ident_mask.mask_and_check_all_hit set a);
           OUnit.assert_bool __LOC__
             (Hash_set_ident_mask.mask_and_check_all_hit set b);
           Hash_set_ident_mask.iter_and_unmask set (fun id mask ->
               if id.Ident.name = "a" then OUnit.assert_bool __LOC__ mask
               else if id.Ident.name = "b" then OUnit.assert_bool __LOC__ mask
               else ());
           OUnit.assert_bool __LOC__
             (not @@ Hash_set_ident_mask.mask_and_check_all_hit set a);
           OUnit.assert_bool __LOC__
             (Hash_set_ident_mask.mask_and_check_all_hit set b) );
         ( __LOC__ >:: fun _ ->
           let len = 1000 in
           let idents =
             Array.init len (fun i -> Ident.create (string_of_int i))
           in
           let set = Hash_set_ident_mask.create 0 in
           Array.iter (fun i -> Hash_set_ident_mask.add_unmask set i) idents;
           for i = 0 to len - 2 do
             OUnit.assert_bool __LOC__
               (not @@ Hash_set_ident_mask.mask_and_check_all_hit set idents.(i))
           done;
           for i = 0 to len - 2 do
             OUnit.assert_bool __LOC__
               (not @@ Hash_set_ident_mask.mask_and_check_all_hit set idents.(i))
           done;
           OUnit.assert_bool __LOC__
             (Hash_set_ident_mask.mask_and_check_all_hit set idents.(len - 1));
           Hash_set_ident_mask.iter_and_unmask set (fun _ _ -> ());
           for i = 0 to len - 2 do
             OUnit.assert_bool __LOC__
               (not @@ Hash_set_ident_mask.mask_and_check_all_hit set idents.(i))
           done;
           for i = 0 to len - 2 do
             OUnit.assert_bool __LOC__
               (not @@ Hash_set_ident_mask.mask_and_check_all_hit set idents.(i))
           done;
           OUnit.assert_bool __LOC__
             (Hash_set_ident_mask.mask_and_check_all_hit set idents.(len - 1))
         );
       ]
