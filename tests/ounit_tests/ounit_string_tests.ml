let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump

let printer_string x = x

let string_eq = OUnit.assert_equal ~printer:(fun id -> id)

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "not found " (Ext_string.rindex_neg "hello" 'x' < 0)
         );
         ( __LOC__ >:: fun _ ->
           Ext_string.rindex_neg "hello" 'h' =~ 0;
           Ext_string.rindex_neg "hello" 'e' =~ 1;
           Ext_string.rindex_neg "hello" 'l' =~ 3;
           Ext_string.rindex_neg "hello" 'l' =~ 3;
           Ext_string.rindex_neg "hello" 'o' =~ 4 );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "empty string" (Ext_string.rindex_neg "" 'x' < 0)
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (not
                (Ext_string.for_all_from "xABc" 1 (function
                  | 'A' .. 'Z' -> true
                  | _ -> false)));
           OUnit.assert_bool __LOC__
             (Ext_string.for_all_from "xABC" 1 (function
               | 'A' .. 'Z' -> true
               | _ -> false));
           OUnit.assert_bool __LOC__
             (Ext_string.for_all_from "xABC" 1_000 (function
               | 'A' .. 'Z' -> true
               | _ -> false)) );
         ( __LOC__ >:: fun _ ->
           Ext_string.starts_with "ab" "a" =~ true;
           Ext_string.starts_with "ab" "" =~ true;
           Ext_string.starts_with "abb" "abb" =~ true;
           Ext_string.starts_with "abb" "abbc" =~ false );
         ( __LOC__ >:: fun _ ->
           Ext_string.for_all "____" (function
             | '_' -> true
             | _ -> false)
           =~ true;
           Ext_string.for_all "___-" (function
             | '_' -> true
             | _ -> false)
           =~ false;
           Ext_string.for_all "" (function
             | '_' -> true
             | _ -> false)
           =~ true );
         ( __LOC__ >:: fun _ ->
           Ext_string.tail_from "ghsogh" 1 =~ "hsogh";
           Ext_string.tail_from "ghsogh" 0 =~ "ghsogh" );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Ext_string.replace_backward_slash "a:\\b\\d" = "a:/b/d");
           OUnit.assert_bool __LOC__
             (Ext_string.replace_backward_slash "a:\\b\\d\\" = "a:/b/d/");
           OUnit.assert_bool __LOC__
             (let old = "a:bd" in
              Ext_string.replace_backward_slash old == old) );
         ( __LOC__ >:: fun _ ->
           string_eq (Ext_filename.new_extension "a.c" ".xx") "a.xx";
           string_eq (Ext_filename.new_extension "abb.c" ".xx") "abb.xx";
           string_eq (Ext_filename.new_extension ".c" ".xx") ".xx";
           string_eq (Ext_filename.new_extension "a/b" ".xx") "a/b.xx";
           string_eq (Ext_filename.new_extension "a/b." ".xx") "a/b.xx" );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__ (Ext_string.compare "" "" = 0);
           OUnit.assert_bool __LOC__ (Ext_string.compare "0" "0" = 0);
           OUnit.assert_bool __LOC__ (Ext_string.compare "" "acd" < 0);
           OUnit.assert_bool __LOC__ (Ext_string.compare "acd" "" > 0);
           for i = 0 to 256 do
             let a = String.init i (fun _ -> '0') in
             let b = String.init i (fun _ -> '0') in
             OUnit.assert_bool __LOC__ (Ext_string.compare b a = 0);
             OUnit.assert_bool __LOC__ (Ext_string.compare a b = 0)
           done;
           for i = 0 to 256 do
             let a = String.init i (fun _ -> '0') in
             let b = String.init i (fun _ -> '0') ^ "\000" in
             OUnit.assert_bool __LOC__ (Ext_string.compare a b < 0);
             OUnit.assert_bool __LOC__ (Ext_string.compare b a > 0)
           done );
         ( __LOC__ >:: fun _ ->
           let slow_compare x y =
             let x_len = String.length x in
             let y_len = String.length y in
             if x_len = y_len then String.compare x y
             else Stdlib.compare x_len y_len
           in
           let same_sign x y =
             if x = 0 then y = 0 else if x < 0 then y < 0 else y > 0
           in
           for _ = 0 to 3000 do
             let chars = [|'a'; 'b'; 'c'; 'd'|] in
             let x = Ounit_data_random.random_string chars 129 in
             let y = Ounit_data_random.random_string chars 129 in
             let a = Ext_string.compare x y in
             let b = slow_compare x y in
             if same_sign a b then OUnit.assert_bool __LOC__ true
             else
               failwith
                 ("incosistent " ^ x ^ " " ^ y ^ " " ^ string_of_int a ^ " "
                ^ string_of_int b)
           done );
         ( __LOC__ >:: fun _ ->
           Ext_namespace.namespace_of_package_name "bs-json" =~ "BsJson" );
         ( __LOC__ >:: fun _ ->
           Ext_namespace.namespace_of_package_name "xx" =~ "Xx" );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal ~printer:(fun x -> x) in
           Ext_namespace.namespace_of_package_name "reason-react"
           =~ "ReasonReact";
           Ext_namespace.namespace_of_package_name "Foo_bar" =~ "Foo_bar";
           Ext_namespace.namespace_of_package_name "reason" =~ "Reason";
           Ext_namespace.namespace_of_package_name "@aa/bb" =~ "AaBb";
           Ext_namespace.namespace_of_package_name "@A/bb" =~ "ABb" );
         ( __LOC__ >:: fun _ ->
           Ext_namespace.change_ext_ns_suffix "a-b" Literals.suffix_js =~ "a.js";
           Ext_namespace.change_ext_ns_suffix "a-" Literals.suffix_js =~ "a.js";
           Ext_namespace.change_ext_ns_suffix "a--" Literals.suffix_js
           =~ "a-.js";
           Ext_namespace.change_ext_ns_suffix "AA-b" Literals.suffix_js
           =~ "AA.js";
           Ext_namespace.js_name_of_modulename "AA-b" Little Literals.suffix_js
           =~ "aA.js";
           Ext_namespace.js_name_of_modulename "AA-b" Upper Literals.suffix_js
           =~ "AA.js";
           Ext_namespace.js_name_of_modulename "AA-b" Upper ".bs.js"
           =~ "AA.bs.js" );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) =
             OUnit.assert_equal ~printer:(fun x ->
                 match x with
                 | None -> ""
                 | Some (a, b) -> a ^ "," ^ b)
           in
           Ext_namespace.try_split_module_name "Js-X" =~ Some ("X", "Js");
           Ext_namespace.try_split_module_name "Js_X" =~ None );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal ~printer:(fun x -> x) in
           let f = Ext_string.capitalize_ascii in
           f "x" =~ "X";
           f "X" =~ "X";
           f "" =~ "";
           f "abc" =~ "Abc";
           f "_bc" =~ "_bc";
           let v = "bc" in
           f v =~ "Bc";
           v =~ "bc" );
         ( __LOC__ >:: fun _ ->
           let k = Ext_modulename.js_id_name_of_hint_name in
           k "xx" =~ "Xx";
           k "react-dom" =~ "ReactDom";
           k "a/b/react-dom" =~ "ReactDom";
           k "a/b" =~ "B";
           k "a/" =~ "A/";
           (*TODO: warning?*)
           k "#moduleid" =~ "Moduleid";
           k "@bundle" =~ "Bundle";
           k "xx#bc" =~ "Xxbc";
           k "hi@myproj" =~ "Himyproj";
           k "ab/c/xx.b.js" =~ "XxBJs";
           (* improve it in the future*)
           k "c/d/a--b" =~ "AB";
           k "c/d/ac--" =~ "Ac" );
         ( __LOC__ >:: fun _ ->
           Ext_string.capitalize_sub "ab-Ns.cmi" 2 =~ "Ab";
           Ext_string.capitalize_sub "Ab-Ns.cmi" 2 =~ "Ab";
           Ext_string.capitalize_sub "Ab-Ns.cmi" 3 =~ "Ab-" );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (String.length (Digest.string ""))
             Ext_digest.length );
         ( __LOC__ >:: fun _ ->
           string_eq (Ext_filename.module_name "a/b/c.d") "C";
           string_eq (Ext_filename.module_name "a/b/xc.res") "Xc";
           string_eq (Ext_filename.module_name "a/b/xc.resi") "Xc";
           string_eq (Ext_filename.module_name "a/b/xc.ml") "Xc";
           string_eq (Ext_filename.module_name "a/b/xc.mli") "Xc";
           string_eq
             (Ext_filename.module_name "a/b/xc.generated.mli")
             "Xc.generated";
           string_eq
             (Ext_filename.module_name "a/b/xc.generated.")
             "Xc.generated";
           string_eq (Ext_filename.module_name "a/b/xc..") "Xc.";
           string_eq (Ext_filename.module_name "a/b/Xc..") "Xc.";
           string_eq (Ext_filename.module_name "a/b/.") "" );
         ( __LOC__ >:: fun _ ->
           Ext_string.split "" ':' =~ [];
           Ext_string.split "a:b:" ':' =~ ["a"; "b"];
           Ext_string.split "a:b:" ':' ~keep_empty:true =~ ["a"; "b"; ""] );
       ]
