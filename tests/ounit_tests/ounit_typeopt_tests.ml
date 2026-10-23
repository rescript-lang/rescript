let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "missing type declaration keeps option wrapping" >:: fun _ ->
           let typ =
             Btype.newgenty
               (Types.Tconstr
                  (Path.Pident (Ident.create "missing"), [], ref Types.Mnil))
           in
           OUnit.assert_equal false
             (Typeopt.type_cannot_contain_undefined typ Env.empty) );
         ( "builtin integers still omit option wrapping" >:: fun _ ->
           OUnit.assert_equal true
             (Typeopt.type_cannot_contain_undefined (Predef.type_int ())
                Env.empty) );
       ]
