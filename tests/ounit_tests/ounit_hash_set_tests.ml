let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

type id = {name: string; stamp: int}

module Id_hash_set = Hash_set.Make (struct
  type t = id
  let equal x y = x.stamp = y.stamp && x.name = y.name
  let hash x = Hashtbl.hash x.stamp
end)

let const_tbl =
  [|
    "0";
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "10";
    "100";
    "99";
    "98";
    "97";
    "96";
    "95";
    "94";
    "93";
    "92";
    "91";
    "90";
    "89";
    "88";
    "87";
    "86";
    "85";
    "84";
    "83";
    "82";
    "81";
    "80";
    "79";
    "78";
    "77";
    "76";
    "75";
    "74";
    "73";
    "72";
    "71";
    "70";
    "69";
    "68";
    "67";
    "66";
    "65";
    "64";
    "63";
    "62";
    "61";
    "60";
    "59";
    "58";
    "57";
    "56";
    "55";
    "54";
    "53";
    "52";
    "51";
    "50";
    "49";
    "48";
    "47";
    "46";
    "45";
    "44";
    "43";
    "42";
    "41";
    "40";
    "39";
    "38";
    "37";
    "36";
    "35";
    "34";
    "33";
    "32";
    "31";
    "30";
    "29";
    "28";
    "27";
    "26";
    "25";
    "24";
    "23";
    "22";
    "21";
    "20";
    "19";
    "18";
    "17";
    "16";
    "15";
    "14";
    "13";
    "12";
    "11";
  |]
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           let v = Hash_set_poly.create 31 in
           for i = 0 to 1000 do
             Hash_set_poly.add v i
           done;
           OUnit.assert_equal (Hash_set_poly.length v) 1001 );
         ( __LOC__ >:: fun _ ->
           let v = Hash_set_poly.create 31 in
           for _ = 0 to 1_0_000 do
             Hash_set_poly.add v 0
           done;
           OUnit.assert_equal (Hash_set_poly.length v) 1 );
         ( __LOC__ >:: fun _ ->
           let v = Hash_set_poly.create 30 in
           for i = 0 to 2_000 do
             Hash_set_poly.add v {name = "x"; stamp = i}
           done;
           for i = 0 to 2_000 do
             Hash_set_poly.add v {name = "x"; stamp = i}
           done;
           for i = 0 to 2_000 do
             assert (Hash_set_poly.mem v {name = "x"; stamp = i})
           done;
           OUnit.assert_equal (Hash_set_poly.length v) 2_001;
           for i = 1990 to 3_000 do
             Hash_set_poly.remove v {name = "x"; stamp = i}
           done;
           OUnit.assert_equal (Hash_set_poly.length v) 1990
         (* OUnit.assert_equal (Hash_set.stats v) *)
         (*   {Hashtbl.num_bindings = 1990; num_buckets = 1024; max_bucket_length = 7; *)
         (*    bucket_histogram = [|139; 303; 264; 178; 93; 32; 12; 3|]} *) );
         ( __LOC__ >:: fun _ ->
           let v = Id_hash_set.create 30 in
           for i = 0 to 2_000 do
             Id_hash_set.add v {name = "x"; stamp = i}
           done;
           for i = 0 to 2_000 do
             Id_hash_set.add v {name = "x"; stamp = i}
           done;
           for i = 0 to 2_000 do
             assert (Id_hash_set.mem v {name = "x"; stamp = i})
           done;
           OUnit.assert_equal (Id_hash_set.length v) 2_001;
           for i = 1990 to 3_000 do
             Id_hash_set.remove v {name = "x"; stamp = i}
           done;
           OUnit.assert_equal (Id_hash_set.length v) 1990;
           for i = 1000 to 3990 do
             Id_hash_set.remove v {name = "x"; stamp = i}
           done;
           OUnit.assert_equal (Id_hash_set.length v) 1000;
           for i = 1000 to 1100 do
             Id_hash_set.add v {name = "x"; stamp = i}
           done;
           OUnit.assert_equal (Id_hash_set.length v) 1101;
           for i = 0 to 1100 do
             OUnit.assert_bool "exist"
               (Id_hash_set.mem v {name = "x"; stamp = i})
           done
         (* OUnit.assert_equal (Hash_set.stats v) *)
         (*   {num_bindings = 1990; num_buckets = 1024; max_bucket_length = 8; *)
         (*    bucket_histogram = [|148; 275; 285; 182; 95; 21; 14; 2; 2|]} *)
         );
         ( __LOC__ >:: fun _ ->
           let duplicate arr =
             let len = Array.length arr in
             let rec aux tbl off =
               if off >= len then None
               else
                 let curr = Array.unsafe_get arr off in
                 if Hash_set_string.check_add tbl curr then aux tbl (off + 1)
                 else Some curr
             in
             aux (Hash_set_string.create len) 0
           in
           let v = [|"if"; "a"; "b"; "c"|] in
           OUnit.assert_equal (duplicate v) None;
           OUnit.assert_equal
             (duplicate [|"if"; "a"; "b"; "b"; "c"|])
             (Some "b") );
         ( __LOC__ >:: fun _ ->
           let of_array lst =
             let len = Array.length lst in
             let tbl = Hash_set_string.create len in
             Ext_array.iter lst (Hash_set_string.add tbl);
             tbl
           in
           let hash = of_array const_tbl in
           let len = Hash_set_string.length hash in
           Hash_set_string.remove hash "x";
           OUnit.assert_equal len (Hash_set_string.length hash);
           Hash_set_string.remove hash "0";
           OUnit.assert_equal (len - 1) (Hash_set_string.length hash) );
       ]
