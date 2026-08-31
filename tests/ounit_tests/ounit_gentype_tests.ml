open OUnit

let suites =
  "gentype"
  >::: [
         ( "escape semantic import paths" >:: fun _ ->
           let emit path =
             path |> Import_path.from_string_unsafe |> Import_path.emit
           in
           assert_equal "./foo\\\\bar" (emit "./foo\\bar");
           assert_equal "./foo\\'bar" (emit "./foo'bar");
           assert_equal "./foo\\nbar" (emit "./foo\nbar") );
         ( "escape semantic TypeScript strings" >:: fun _ ->
           let escape = Emit_text.escape_string_contents in
           assert_equal "é" (escape "é");
           assert_equal "a\\\"b\\\\c" (escape "a\"b\\c");
           assert_equal "\\b\\f\\n\\r\\t\\x0b\\x7f"
             (escape "\b\012\n\r\t\011\127") );
       ]
