open Lsp.Types
open Rescript_language_server_core.Document

let%expect_test "range_of_text" =
  let print text =
    let range = range_of_text text in
    Yojson.Safe.pretty_to_string
      (`Assoc
         [
           ("start", range.start |> Position.yojson_of_t);
           ("end", range.end_ |> Position.yojson_of_t);
         ])
    |> print_endline
  in
  print "";
  print "let a = 1";
  print "let a = 1\nlet b = 2";
  print "let a = 1\n";
  print "\xC3\xA9";
  print "ab";
  print "ab\ncd";
  [%expect
    {|
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 0, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 9, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 9, "line": 1 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 0, "line": 1 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 1 }
    }
    |}]
