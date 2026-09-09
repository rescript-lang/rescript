let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "platform implementations only use ReScript source files" >:: fun _ ->
           let platforms = ["android"; "ios"] in
           OUnit.assert_equal
             (Some ("android", "Button"))
             (Analysis.Find_files.get_platform_implementation platforms
                "src/Button.android.res");
           OUnit.assert_equal None
             (Analysis.Find_files.get_platform_implementation platforms
                "src/Button.android.re");
           OUnit.assert_equal None
             (Analysis.Find_files.get_platform_implementation platforms
                "src/Button.android.ml") );
       ]
