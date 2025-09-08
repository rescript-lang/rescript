let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let printer_string x = x
let ( =~ ) = OUnit.assert_equal ~printer:printer_string

let scope_test s (a, b, c) =
  match Bsb_pkg_types.extract_pkg_name_and_file s with
  | Scope (a0, b0), c0 ->
    a =~ a0;
    b =~ b0;
    c =~ c0
  | Global _, _ -> OUnit.assert_failure __LOC__

let global_test s (a, b) =
  match Bsb_pkg_types.extract_pkg_name_and_file s with
  | Scope _, _ -> OUnit.assert_failure __LOC__
  | Global a0, b0 ->
    a =~ a0;
    b =~ b0

let s_test0 s (a, b) =
  match Bsb_pkg_types.string_as_package s with
  | Scope (name, scope) ->
    a =~ name;
    b =~ scope
  | _ -> OUnit.assert_failure __LOC__

let s_test1 s a =
  match Bsb_pkg_types.string_as_package s with
  | Global x -> a =~ x
  | _ -> OUnit.assert_failure __LOC__

let group0 =
  Map_string.of_list
    [
      ( "Liba",
        {
          Bsb_db.info = Impl_intf;
          dir = "a";
          case = false;
          name_sans_extension = "liba";
        } );
    ]
let group1 =
  Map_string.of_list
    [
      ( "Ciba",
        {
          Bsb_db.info = Impl_intf;
          dir = "b";
          case = false;
          name_sans_extension = "liba";
        } );
    ]

let parse_db db : Bsb_db_decode.t =
  let buf = Ext_buffer.create 10_000 in
  Bsb_db_encode.encode db buf;
  let s = Ext_buffer.contents buf in
  Bsb_db_decode.decode s

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           scope_test "@hello/hi" ("hi", "@hello", "");

           scope_test "@hello/hi/x" ("hi", "@hello", "x");

           scope_test "@hello/hi/x/y" ("hi", "@hello", "x/y") );
         ( __LOC__ >:: fun _ ->
           global_test "hello" ("hello", "");
           global_test "hello/x" ("hello", "x");
           global_test "hello/x/y" ("hello", "x/y") );
         ( __LOC__ >:: fun _ ->
           s_test0 "@x/y" ("y", "@x");
           s_test0 "@x/y/z" ("y/z", "@x");
           s_test1 "xx" "xx";
           s_test1 "xx/yy/zz" "xx/yy/zz" );
         ( __LOC__ >:: fun _ ->
           match parse_db {lib = group0; dev = group1} with
           | {
            lib = Group {modules = [|"Liba"|]};
            dev = Group {modules = [|"Ciba"|]};
           } ->
             OUnit.assert_bool __LOC__ true
           | _ -> OUnit.assert_failure __LOC__ );
         ( __LOC__ >:: fun _ ->
           match parse_db {lib = group0; dev = Map_string.empty} with
           | {lib = Group {modules = [|"Liba"|]}; dev = Dummy} ->
             OUnit.assert_bool __LOC__ true
           | _ -> OUnit.assert_failure __LOC__ );
         ( __LOC__ >:: fun _ ->
           match parse_db {lib = Map_string.empty; dev = group1} with
           | {lib = Dummy; dev = Group {modules = [|"Ciba"|]}} ->
             OUnit.assert_bool __LOC__ true
           | _ -> OUnit.assert_failure __LOC__ )
         (* __LOC__ >:: begin fun _ ->
            OUnit.assert_equal parse_data_one  data_one
            end ;
            __LOC__ >:: begin fun _ ->

            OUnit.assert_equal parse_data_two data_two
            end *);
       ]
