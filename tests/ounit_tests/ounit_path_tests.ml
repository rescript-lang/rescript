let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let normalize = Ext_path.normalize_absolute_path
let ( =~ ) x y =
  OUnit.assert_equal
    ~printer:(fun x -> x)
    ~cmp:(fun x y -> Ext_string.equal x y)
    x y

let suites =
  __FILE__
  >::: [
         ( "linux path tests" >:: fun _ ->
           let norm =
             Array.map normalize
               [|
                 "/gsho/./..";
                 "/a/b/../c../d/e/f";
                 "/a/b/../c/../d/e/f";
                 "/gsho/./../..";
                 "/a/b/c/d";
                 "/a/b/c/d/";
                 "/a/";
                 "/a";
                 "/a.txt/";
                 "/a.txt";
               |]
           in
           OUnit.assert_equal norm
             [|
               "/";
               "/a/c../d/e/f";
               "/a/d/e/f";
               "/";
               "/a/b/c/d";
               "/a/b/c/d";
               "/a";
               "/a";
               "/a.txt";
               "/a.txt";
             |] );
         ( __LOC__ >:: fun _ ->
           normalize "/./a/.////////j/k//../////..///././b/./c/d/./."
           =~ "/a/b/c/d" );
         ( __LOC__ >:: fun _ ->
           normalize "/./a/.////////j/k//../////..///././b/./c/d/././../"
           =~ "/a/b/c" );
         ( __LOC__ >:: fun _ ->
           let aux a b result =
             Ext_path.rel_normalized_absolute_path ~from:a b =~ result;

             Ext_path.rel_normalized_absolute_path
               ~from:(String.sub a 0 (String.length a - 1))
               b
             =~ result;

             Ext_path.rel_normalized_absolute_path ~from:a
               (String.sub b 0 (String.length b - 1))
             =~ result;

             Ext_path.rel_normalized_absolute_path
               ~from:(String.sub a 0 (String.length a - 1))
               (String.sub b 0 (String.length b - 1))
             =~ result
           in
           aux "/a/b/c/" "/a/b/c/d/" "./d";
           aux "/a/b/c/" "/a/b/c/d/e/f/" "./d/e/f";
           aux "/a/b/c/d/" "/a/b/c/" "..";
           aux "/a/b/c/d/" "/a/b/" "../..";
           aux "/a/b/c/d/" "/a/" "../../..";
           aux "/a/b/c/d/" "//" "../../../.." );
         (* This is still correct just not optimal depends
            on user's perspective *)
         ( __LOC__ >:: fun _ ->
           Ext_path.rel_normalized_absolute_path ~from:"/a/b/c/d" "/x/y"
           =~ "../../../../x/y" );
         (* used in module system: [es6-global] and [amdjs-global] *)
         ( __LOC__ >:: fun _ ->
           Ext_path.rel_normalized_absolute_path
             ~from:"/usr/local/lib/node_modules/" "//"
           =~ "../../../..";
           Ext_path.rel_normalized_absolute_path
             ~from:"/usr/local/lib/node_modules/" "/"
           =~ "../../../..";
           Ext_path.rel_normalized_absolute_path ~from:"./"
             "./node_modules/xx/./xx.js"
           =~ "./node_modules/xx/xx.js";
           Ext_path.rel_normalized_absolute_path ~from:"././"
             "./node_modules/xx/./xx.js"
           =~ "./node_modules/xx/xx.js" );
         ( __LOC__ >:: fun _ ->
           Ext_path.node_rebase_file ~to_:"lib/js/src/a" ~from:"lib/js/src" "b"
           =~ "./a/b";
           Ext_path.node_rebase_file ~to_:"lib/js/src/" ~from:"lib/js/src" "b"
           =~ "./b";
           Ext_path.node_rebase_file ~to_:"lib/js/src" ~from:"lib/js/src/a" "b"
           =~ "../b";
           Ext_path.node_rebase_file ~to_:"lib/js/src/a" ~from:"lib/js/" "b"
           =~ "./src/a/b";
           Ext_path.node_rebase_file ~to_:"lib/js/./src/a" ~from:"lib/js/src/a/"
             "b"
           =~ "./b";

           Ext_path.node_rebase_file ~to_:"lib/js/src/a" ~from:"lib/js/src/a/"
             "b"
           =~ "./b";
           Ext_path.node_rebase_file ~to_:"lib/js/src/a/" ~from:"lib/js/src/a/"
             "b"
           =~ "./b" );
       ]
