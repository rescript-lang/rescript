let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "inspection and consumption have separate boundaries" >:: fun _ ->
           let p =
             Res_parser.make "let /* keep */ x = \"unterminated" "test.res"
           in
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           OUnit.assert_equal [] p.diagnostics;
           OUnit.assert_equal
             (Res_token.Let {unwrap = false})
             (Res_parser.peek p);
           OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek2 p);
           OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek2 p);
           OUnit.assert_equal [] p.comments;
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           Res_parser.next p;
           OUnit.assert_equal 3 (Res_parser.position p).pos_cnum;
           OUnit.assert_equal [] p.comments;
           OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek p);
           Res_parser.next p;
           OUnit.assert_equal 1 (List.length p.comments);
           Res_parser.expect Res_token.Equal p;
           OUnit.assert_equal [] p.diagnostics;
           ignore (Res_parser.peek p);
           OUnit.assert_equal [] p.diagnostics;
           Res_parser.next p;
           OUnit.assert_equal 1 (List.length p.diagnostics) );
         ( "inspection leaves the consumed position before trivia" >:: fun _ ->
           let p =
             Res_parser.make "foo(x) /* keep */\r\n+ y /* tail */\r\n"
               "test.res"
           in
           List.iter
             (fun token ->
               OUnit.assert_equal token (Res_parser.peek p);
               Res_parser.next p)
             Res_token.[Lident "foo"; Lparen; Lident "x"; Rparen];
           let position = Res_parser.position p in
           OUnit.assert_equal 6 position.pos_cnum;
           OUnit.assert_equal 1 position.pos_lnum;
           OUnit.assert_equal Res_token.Plus (Res_parser.peek p);
           OUnit.assert_equal (Res_token.Lident "y") (Res_parser.peek2 p);
           OUnit.assert_equal 2 (Res_parser.start_pos p).pos_lnum;
           ignore (Res_parser.end_pos p);
           OUnit.assert_equal position (Res_parser.position p);
           OUnit.assert_equal [] p.comments;
           Res_parser.next p;
           Res_parser.next p;
           let position = Res_parser.position p in
           OUnit.assert_equal 2 position.pos_lnum;
           OUnit.assert_equal Res_token.Eof (Res_parser.peek p);
           Res_parser.finish p;
           OUnit.assert_equal position (Res_parser.position p);
           OUnit.assert_equal 2 (List.length p.comments) );
         ( "progress uses the consumed position across UTF-16 and rollback"
         >:: fun _ ->
           let p = Res_parser.make "\"😀\"\r\nx" "test.res" in
           let position = Res_parser.position p in
           ignore (Res_parser.peek2 p);
           OUnit.assert_equal None
             (Res_parser.check_progress ~position ~result:() p);
           Res_parser.next p;
           OUnit.assert_equal (Some ())
             (Res_parser.check_progress ~position ~result:() p);
           let position = Res_parser.position p in
           OUnit.assert_equal 4 position.pos_cnum;
           OUnit.assert_equal 1 position.pos_lnum;
           Res_parser.lookahead p (fun p -> Res_parser.next p);
           OUnit.assert_equal position (Res_parser.position p);
           OUnit.assert_equal None
             (Res_parser.check_progress ~position ~result:() p);
           Res_parser.next p;
           OUnit.assert_equal (Some ())
             (Res_parser.check_progress ~position ~result:() p);
           let position = Res_parser.position p in
           OUnit.assert_equal 9 position.pos_cnum;
           OUnit.assert_equal 2 position.pos_lnum;
           OUnit.assert_equal 8 position.pos_bol;
           Res_parser.next_unsafe p;
           OUnit.assert_equal None
             (Res_parser.check_progress ~position ~result:() p) );
         ( "EOF retains trailing trivia exactly once" >:: fun _ ->
           let p = Res_parser.make "let x = 1 /* tail */" "test.res" in
           ignore (Res_core.parse_implementation p);
           OUnit.assert_equal Res_token.Eof (Res_parser.peek2 p);
           OUnit.assert_equal 1 (List.length p.comments);
           Res_parser.finish p;
           OUnit.assert_equal 1 (List.length p.comments) );
         ( "speculation restores recovery, warnings and exceptions" >:: fun _ ->
           let p = Res_parser.make "/* keep */ x y" "test.res" in
           let report p = Res_parser.err p (Res_diagnostics.message "probe") in
           let probe p =
             Res_parser.begin_region p;
             report p;
             Res_parser.end_region p;
             report p;
             Res_parser.warn p Location.none
               (Warnings.Deprecated
                  ("probe", Location.none, Location.none, false));
             ignore (Res_parser.peek2 p);
             Res_parser.next p;
             raise Exit
           in
           OUnit.assert_raises Exit (fun () -> Res_parser.lookahead p probe);
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           OUnit.assert_equal [] p.diagnostics;
           OUnit.assert_equal [] p.comments;
           OUnit.assert_equal [] p.warnings;
           OUnit.assert_equal [Res_parser.Report] p.regions;
           OUnit.assert_equal None
             (Res_parser.try_parse p (fun p ->
                  Res_parser.next p;
                  report p;
                  None));
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           OUnit.assert_equal (Some ())
             (Res_parser.try_parse p (fun p ->
                  Res_parser.next p;
                  report p;
                  Some ()));
           OUnit.assert_equal 1 (List.length p.comments);
           OUnit.assert_equal 1 (List.length p.diagnostics);
           OUnit.assert_equal [Res_parser.Silent] p.regions );
         ( "raw readers discard ordinary lookahead" >:: fun _ ->
           let p = Res_parser.make "/.foo/g x" "test.res" in
           ignore (Res_parser.peek2 p);
           Res_parser.next_regex_token p;
           OUnit.assert_equal
             (Res_token.Regex (".foo", "g"))
             (Res_parser.peek p);
           OUnit.assert_equal 0 (Res_parser.start_pos p).pos_cnum;
           OUnit.assert_equal 7 (Res_parser.end_pos p).pos_cnum;
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           Res_parser.next p;
           OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek p);
           let p = Res_parser.make "`\"raw ${x}}tail` after" "test.res" in
           ignore (Res_parser.peek2 p);
           Res_parser.next_template_literal_token p;
           (match Res_parser.peek p with
           | TemplatePart (text, _) -> OUnit.assert_equal "\"raw " text
           | _ -> OUnit.assert_failure "expected template part");
           Res_parser.next p;
           Res_parser.next p;
           OUnit.assert_equal Res_token.Rbrace (Res_parser.peek p);
           ignore (Res_parser.peek2 p);
           Res_parser.next_template_literal_token p;
           (match Res_parser.peek p with
           | TemplateTail (text, _) -> OUnit.assert_equal "}tail" text
           | _ -> OUnit.assert_failure "expected template tail");
           Res_parser.next p;
           OUnit.assert_equal (Res_token.Lident "after") (Res_parser.peek p);
           OUnit.assert_equal [] p.diagnostics );
         ( "lookahead restores UTF-16 and CRLF positions" >:: fun _ ->
           let p = Res_parser.make "\"😀\"\r\n/.x/g name" "test.res" in
           Res_parser.next p;
           OUnit.assert_equal 4 (Res_parser.position p).pos_cnum;
           let start = Res_parser.start_pos p in
           OUnit.assert_equal 2 start.pos_lnum;
           OUnit.assert_equal 8 start.pos_bol;
           Res_parser.lookahead p (fun p ->
               ignore (Res_parser.peek2 p);
               Res_parser.next_regex_token p;
               Res_parser.next p;
               Res_parser.next p);
           OUnit.assert_equal start (Res_parser.start_pos p);
           Res_parser.next_regex_token p;
           Res_parser.next p;
           OUnit.assert_equal 13 (Res_parser.position p).pos_cnum;
           OUnit.assert_equal 14 (Res_parser.start_pos p).pos_cnum );
         ( "nested transactions preserve the outer checkpoint" >:: fun _ ->
           let p = Res_parser.make "x /* keep */ y z" "test.res" in
           OUnit.assert_equal (Res_token.Lident "y") (Res_parser.peek2 p);
           Res_parser.lookahead p (fun p ->
               Res_parser.next p;
               Res_parser.lookahead p (fun p ->
                   Res_parser.next p;
                   OUnit.assert_equal (Res_token.Lident "z") (Res_parser.peek p));
               OUnit.assert_equal (Res_token.Lident "y") (Res_parser.peek p);
               OUnit.assert_equal [] p.comments);
           OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek p);
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           Res_parser.next p;
           Res_parser.next p;
           OUnit.assert_equal 1 (List.length p.comments) );
         ( "type argument hints survive speculative parsing" >:: fun _ ->
           let p =
             Res_parser.make "type t<'a> = Nullable.t('a)" "Recovery.res"
           in
           let inspect p =
             ignore (Res_core.parse_implementation p);
             List.map Res_diagnostics.explain p.diagnostics
           in
           let expected =
             ["Type parameters require angle brackets:\n  Nullable.t<'a>"]
           in
           OUnit.assert_equal expected (Res_parser.lookahead p inspect);
           OUnit.assert_equal [] p.diagnostics;
           OUnit.assert_equal expected (inspect p) );
         ( "regex opening position survives a non-BMP prefix at EOF" >:: fun _ ->
           let p = Res_parser.make "\"😀\"; /." "test.res" in
           Res_parser.next p;
           Res_parser.next p;
           let opening = Res_parser.start_pos p in
           OUnit.assert_equal 6 opening.pos_cnum;
           Res_parser.next_regex_token p;
           OUnit.assert_equal opening (Res_parser.start_pos p);
           Res_parser.next p;
           OUnit.assert_equal 10 (Res_parser.position p).pos_cnum );
         ( "context-independent angle tokens" >:: fun _ ->
           let scanner = Res_scanner.make ~filename:"test.res" ">>= <<" in
           let rec tokens acc =
             let _, _, token = Res_scanner.scan scanner in
             if token = Res_token.Eof then List.rev acc
             else tokens (token :: acc)
           in
           OUnit.assert_equal
             Res_token.[GreaterThan; GreaterThan; Equal; LessThan; LessThan]
             (tokens []) );
         ( "operator spans and following token" >:: fun _ ->
           List.iter
             (fun (source, expected, width) ->
               let p = Res_parser.make source "test.res" in
               ignore (Res_parser.peek2 p);
               OUnit.assert_equal expected (Res_parser.peek_binary_operator p);
               OUnit.assert_equal 0 (Res_parser.start_pos p).pos_cnum;
               OUnit.assert_equal width (Res_parser.end_pos p).pos_cnum;
               Res_parser.next p;
               OUnit.assert_equal (Res_token.Lident "x") (Res_parser.peek p);
               OUnit.assert_equal width (Res_parser.position p).pos_cnum;
               OUnit.assert_equal (width + 1) (Res_parser.start_pos p).pos_cnum)
             Res_token.
               [
                 (">= x", GreaterEqual, 2);
                 (">> x", RightShift, 2);
                 (">>> x", RightShiftUnsigned, 3);
                 ("<< x", LeftShift, 2);
               ] );
         ( "trivia does not join operators" >:: fun _ ->
           List.iter
             (fun source ->
               let p = Res_parser.make source "test.res" in
               let position = Res_parser.position p in
               OUnit.assert_equal Res_token.GreaterThan
                 (Res_parser.peek_binary_operator p);
               OUnit.assert_equal position (Res_parser.position p))
             ["> >"; ">\n>"; ">/* comment */>"; "> ="] );
         ( "lookahead restores operator and regex cursors" >:: fun _ ->
           let p = Res_parser.make ">>> /* keep */ /[<>]/g" "test.res" in
           let inspect p =
             OUnit.assert_equal Res_token.RightShiftUnsigned
               (Res_parser.peek_binary_operator p);
             Res_parser.next p;
             OUnit.assert_equal Res_token.Forwardslash (Res_parser.peek p);
             Res_parser.next_regex_token p;
             OUnit.assert_equal
               (Res_token.Regex ("[<>]", "g"))
               (Res_parser.peek p);
             Res_parser.next p;
             OUnit.assert_equal 1 (List.length p.comments)
           in
           Res_parser.lookahead p inspect;
           OUnit.assert_equal Res_token.GreaterThan (Res_parser.peek p);
           OUnit.assert_equal Lexing.dummy_pos (Res_parser.position p);
           OUnit.assert_equal 1 (Res_parser.end_pos p).pos_cnum;
           OUnit.assert_equal [] p.comments;
           inspect p );
       ]
