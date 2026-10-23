open OUnit

let suites =
  "gentype"
  >::: [
         ( "split relative module paths" >:: fun _ ->
           assert_equal ["."] (Module_resolver.path_to_list ".");
           assert_equal ["src"] (Module_resolver.path_to_list "src");
           assert_equal ["nested"; "src"]
             (Module_resolver.path_to_list (Filename.concat "src" "nested")) );
         ( "stop at Windows drive roots" >:: fun _ ->
           skip_if (not Sys.win32) "Windows drive roots require Windows";
           List.iter
             (fun root ->
               assert_equal [root] (Module_resolver.path_to_list root);
               assert_equal ["nested"; "src"; root]
                 (Module_resolver.path_to_list
                    (Filename.concat root (Filename.concat "src" "nested"))))
             ["C:\\"; "D:\\"] );
         ( "escape semantic import paths" >:: fun _ ->
           let emit path =
             path |> Import_path.from_string_unsafe |> Import_path.emit
           in
           assert_equal "./foo\\\\bar" (emit "./foo\\bar");
           assert_equal "./foo\\'bar" (emit "./foo'bar");
           assert_equal "./foo\\nbar" (emit "./foo\nbar");
           let line_separator = Ext_utf8.encode_codepoint 0x2028 in
           let paragraph_separator = Ext_utf8.encode_codepoint 0x2029 in
           assert_equal "./foo\\u2028bar\\u2029baz"
             (emit
                ("./foo" ^ line_separator ^ "bar" ^ paragraph_separator ^ "baz"))
         );
         ( "escape semantic TypeScript strings" >:: fun _ ->
           let escape = Emit_text.escape_string_contents in
           assert_equal "é" (escape "é");
           assert_equal "a\\\"b\\\\c" (escape "a\"b\\c");
           assert_equal "\\b\\f\\n\\r\\t\\v\\x7F"
             (escape "\b\012\n\r\t\011\127") );
       ]
