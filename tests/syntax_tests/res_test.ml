let data_dir = "tests/syntax_tests/data"

(* test printing of .res file*)
let () =
  let filename = Filename.concat data_dir "api/resSyntax.res" in
  let pretty_source = Res_multi_printer.print filename in
  assert (
    pretty_source
    = {|// test file

if true {
  Console.log("true")
} else {
  Console.log("false")
}
|})

(* test printing of .resi file*)
let () =
  let filename = Filename.concat data_dir "api/resiSyntax.resi" in
  let pretty_source = Res_multi_printer.print filename in
  assert (pretty_source = {|// test interface file

let x: int
|})

(* Formatting keeps a file's line endings, also inside comments, template
   literals and raw JS, and formatting the result again changes nothing. The
   line breaks inside literals are part of the program and keep their bytes,
   whatever the file's line endings, and so do the ones inside comments. *)
let () =
  let to_crlf text = String.concat "\r\n" (String.split_on_char '\n' text) in
  let lf =
    "/* a\n\
    \ multi-line comment */\n\
     /* a comment\n\
    \ * with stars\n\
    \ */\n\
     let s = `line 1\n\
     line 2`\n\
     let r = %raw(`\n\
    \  function () {\n\
    \    return 1\n\
    \  }\n\
     `)\n"
  in
  let format source =
    let filename = Filename.temp_file "line_endings" ".res" in
    Fun.protect
      (fun () ->
        let channel = open_out_bin filename in
        output_string channel source;
        close_out channel;
        Res_multi_printer.print filename)
      ~finally:(fun () -> Sys.remove filename)
  in
  List.iter
    (fun source ->
      let formatted = format source in
      assert (formatted = source);
      assert (format formatted = formatted))
    [
      lf;
      to_crlf lf;
      "let x = 1\r\nlet s = \"a\nb\"\r\n";
      "let x = 1\nlet s = \"a\r\nb\"\n";
      "let x = 1\r\nlet t = `a\nb`\r\nlet c = '\n'\r\nlet p = #\"a\nb\"\r\n";
      "let x = 1\nlet t = `a\r\nb`\nlet c = '\r'\nlet p = #\"a\r\nb\"\n";
      "let x = 1\n/* a\r\r\nb\nc */\nlet y = 2\n";
      "let x = 1\r\n/* a\nb\r\nc */\r\nlet y = 2\r\n";
    ];
  (* Formatting keeps a byte order mark, and a lone CR ends a line *)
  let bom = "\xEF\xBB\xBF" in
  List.iter
    (fun source ->
      let formatted = format source in
      assert (formatted = source);
      assert (format formatted = formatted))
    [bom ^ "let x = 1\n"; bom ^ "// c\r\nlet x = 1\r\n"];
  assert (format "let x = 1 // c\rlet y = 2\n" = "let x = 1 // c\nlet y = 2\n");
  assert (format "let x = 1\rlet y = 2\r" = "let x = 1\nlet y = 2\n");
  (* The printer writes the line breaks of a comment it reindents *)
  let formatted = format "let x = 1\n/* a\r\r\n b */\nlet y = 2\n" in
  assert (formatted = "let x = 1\n/* a\n b */\nlet y = 2\n");
  assert (format (to_crlf formatted) = to_crlf formatted)

let () = print_endline "✅ multi printer api tests"

(* The scanner skips a UTF-8 byte order mark and ends a line at a CR without an
   LF, and code frames show the lines the positions refer to *)
let () =
  let parse source =
    let result =
      Res_driver.parse_implementation_from_source ~display_filename:"bom.res"
        ~source
    in
    assert (not result.invalid);
    result.parsetree
  in
  let position_of_y source =
    match parse source with
    | _ :: {Parsetree.pstr_loc = {loc_start}} :: _ -> loc_start
    | _ -> assert false
  in
  let line_and_column (pos : Lexing.position) =
    (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
  in
  let bom = "\xEF\xBB\xBF" in
  (match parse (bom ^ "let x = 1") with
  | [{pstr_loc = {loc_start}}] -> assert (line_and_column loc_start = (1, 0))
  | _ -> assert false);
  List.iter
    (fun (source, expected) ->
      assert (line_and_column (position_of_y source) = expected))
    [
      ("let x = 1\rlet y = 2", (2, 0));
      ("let x = 1 // c\rlet y = 2", (2, 0));
      ("let x = 1 /* c */\r  let y = 2", (2, 2));
      ("let x = 1\r\nlet y = 2", (2, 0));
      ("let x = 1\r\rlet y = 2", (3, 0));
      ("let x = 1\n\rlet y = 2", (3, 0));
      (bom ^ "let x = 1; let y = 2", (1, 11));
      (bom ^ "let x = 1\rlet y = 2", (2, 0));
    ];
  (* A regex literal ends at a line break *)
  assert
    (Res_driver.parse_implementation_from_source ~display_filename:"regex.res"
       ~source:"let r = /a\rb/")
      .invalid;
  Code_frame.setup (Some Misc.Color.Never);
  let frame source =
    let start_pos = position_of_y source in
    Code_frame.print ~is_warning:false ~src:source ~start_pos
      ~end_pos:{start_pos with pos_cnum = start_pos.pos_cnum + 3}
  in
  assert (frame (bom ^ "let x = 1; let y = 2") = "  1 │ let x = 1; let y = 2\n");
  assert (
    frame "let x = 1\rlet y = 2\rlet z = 3"
    = "  1 │ let x = 1\n  2 │ let y = 2\n  3 │ let z = 3\n");
  print_endline "✅ byte order mark and lone CR"

let () =
  let filename =
    Filename.concat data_dir "printer/comments/callbackTrailing.res"
  in
  let source = Ext_io.load_file filename in
  let parse source =
    let result =
      Res_driver.parse_implementation_from_source ~display_filename:filename
        ~source
    in
    assert (not result.invalid);
    result
  in
  let format ~width result =
    Res_printer.print_implementation ~width result.Res_driver.parsetree
      ~comments:result.comments
  in
  let comment_texts result =
    List.map
      (fun comment -> String.trim (Res_comment.txt comment))
      result.Res_driver.comments
  in
  List.iter
    (fun width ->
      let original = parse source in
      let printed = format ~width original in
      let reparsed = parse printed in
      if comment_texts original <> comment_texts reparsed then
        failwith
          (Printf.sprintf
             "Callback formatting changed comments at width %d.\n\
              Source:\n\
              %s\n\
              Printed:\n\
              %s"
             width source printed);
      let reprinted = format ~width reparsed in
      if printed <> reprinted then
        failwith
          (Printf.sprintf
             "Callback comment formatting is unstable at width %d.\n\
              First pass:\n\
              %s\n\
              Second pass:\n\
              %s"
             width printed reprinted))
    [20; 40; 80; 100; 120];
  print_endline "✅ callback trailing comments are stable at multiple widths"

let () =
  let filename = Filename.concat data_dir "printer/regexpStatements.res" in
  let source = Ext_io.load_file filename in
  let parse source =
    let result =
      Res_driver.parse_implementation_from_source ~display_filename:filename
        ~source
    in
    if result.invalid then
      failwith ("Regexp statements failed to parse:\n" ^ source);
    result
  in
  let original = parse source in
  let comment_texts result =
    List.map Res_comment.txt result.Res_driver.comments
  in
  List.iter
    (fun width ->
      let format result =
        Res_printer.print_implementation ~width result.Res_driver.parsetree
          ~comments:result.comments
      in
      let printed = format original in
      let reparsed = parse printed in
      assert (comment_texts original = comment_texts reparsed);
      let reprinted = format reparsed in
      if printed <> reprinted then
        failwith
          (Printf.sprintf
             "Regexp formatting at width %d:\nFIRST:\n%s\nSECOND:\n%s" width
             printed reprinted))
    [20; 40; 80; 100; 120];
  print_endline "✅ regexp statements round-trip at multiple widths"

let () =
  let without_locations tree =
    let mapper =
      {Ast_mapper.default_mapper with location = (fun _ _ -> Location.none)}
    in
    mapper.structure mapper tree
  in
  let parse source =
    let result =
      Res_driver.parse_implementation_from_source
        ~display_filename:"statement-boundaries.res" ~source
    in
    assert (not result.invalid);
    result
  in
  List.iter
    (fun source ->
      let original = parse source in
      List.iter
        (fun width ->
          let printed =
            Res_printer.print_implementation ~width original.parsetree
              ~comments:original.comments
          in
          let reparsed = parse printed in
          assert (
            without_locations original.parsetree
            = without_locations reparsed.parsetree))
        [20; 40; 80; 100; 120])
    [
      "(@condition truth) ? (@yes first) : (@no second)";
      "let result = (@condition truth) ? (@yes first) : (@no second)";
      "let result = truth ? first : ((@condition other) ? (@yes second) : (@no \
       third))";
      "let f = () => {\nlet before = 0\nopen Foo\n(/a/->ignore)\n}";
      "let f = () => {\nmodule M = Foo\n(/a/->ignore)\n(/b/->ignore)\n}";
      "let f = () => {\nopen Foo\nignore()\n(/a/->ignore)\n}";
      "let before = 0\n@foo (/a/->ignore)";
    ];
  print_endline "✅ statement boundary formatting preserves attribute scope"

(* PPX-generated ## accesses print their receiver before the operator. *)
let () =
  let source = "let t = 0\n(/a/[\"member\"])" in
  let parsed =
    Res_driver.parse_implementation_from_source ~display_filename:"ppx.res"
      ~source
  in
  assert (not parsed.invalid);
  let structure =
    List.map
      (fun item ->
        match item.Parsetree.pstr_desc with
        | Pstr_eval
            (({pexp_desc = Pexp_object_get (receiver, _)} as expr), attrs) ->
          let funct =
            Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "##"))
          in
          let member =
            Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "member"))
          in
          {
            item with
            pstr_desc =
              Pstr_eval
                ( {
                    expr with
                    pexp_desc =
                      (Ast_helper.Exp.apply funct
                         [(Nolabel, receiver); (Nolabel, member)])
                        .pexp_desc;
                  },
                  attrs );
          }
        | _ -> item)
      parsed.parsetree
  in
  let printed =
    Res_printer.print_implementation ~width:100 structure ~comments:[]
  in
  assert (printed = "let t = 0\n(/a/[\"member\"])\n");
  let reparsed =
    Res_driver.parse_implementation_from_source ~display_filename:"ppx.res"
      ~source:printed
  in
  assert (not reparsed.invalid)

let () =
  let filename = Filename.concat data_dir "printer/expr/jsxChildren.res" in
  let source =
    Ext_io.load_file filename
    ^ Ext_io.load_file
        (Filename.concat data_dir "printer/expr/jsxChildrenComments.res")
  in
  let parse source =
    let result =
      Res_driver.parse_implementation_from_source ~display_filename:filename
        ~source
    in
    assert (not result.invalid);
    result
  in
  let format ~width result =
    Res_printer.print_implementation ~width result.Res_driver.parsetree
      ~comments:result.comments
  in
  let comment_texts result =
    List.map
      (fun comment ->
        Res_comment.txt comment |> String.split_on_char '\n'
        |> List.map String.trim |> String.concat "\n" |> String.trim)
      result.Res_driver.comments
  in
  (* Compare the expression structure independently of formatting locations and
     explicit braces introduced by the migration. Keep expression attributes. *)
  let expression_structure result =
    let mapper =
      {
        Ast_mapper.default_mapper with
        expr =
          (fun mapper expr ->
            match expr.Parsetree.pexp_desc with
            | Pexp_braces {expr = inner; braces_loc = _braces_loc} ->
              mapper.expr mapper
                {
                  inner with
                  pexp_attributes = expr.pexp_attributes @ inner.pexp_attributes;
                }
            | _ -> Ast_mapper.default_mapper.expr mapper expr);
      }
    in
    let ast = mapper.structure mapper result.Res_driver.parsetree in
    Format.asprintf "%a" Pprintast.structure ast
  in
  List.iter
    (fun width ->
      let original = parse source in
      let printed = format ~width original in
      let reparsed = parse printed in
      if expression_structure original <> expression_structure reparsed then
        failwith
          (Printf.sprintf "JSX child migration changed expressions at width %d"
             width);
      if comment_texts original <> comment_texts reparsed then
        failwith
          (Printf.sprintf "JSX child migration changed comments at width %d"
             width);
      let reprinted = format ~width reparsed in
      if printed <> reprinted then
        failwith
          (Printf.sprintf
             "JSX child migration is unstable at width %d.\n\
              First pass:\n\
              %s\n\
              Second pass:\n\
              %s"
             width printed reprinted))
    [20; 40; 80; 100; 120];
  assert (
    format ~width:80 (parse "let x = <span> hello </span>")
    = "let x = <span>{hello}</span>\n");
  assert (
    format ~width:80 (parse "let x = <> hello </>") = "let x = <>{hello}</>\n");
  List.iter
    (fun width ->
      List.iter
        (fun (opening, closing) ->
          List.iter
            (fun (comment, formatted_comment) ->
              List.iter
                (fun gap ->
                  let source =
                    "let x = " ^ opening ^ "<A /> " ^ comment ^ "\n" ^ gap
                    ^ "<B />" ^ closing
                  in
                  let expected =
                    "let x =\n  " ^ opening ^ "\n    <A />\n"
                    ^ formatted_comment ^ "\n" ^ gap ^ "    <B />\n  " ^ closing
                    ^ "\n"
                  in
                  let printed = format ~width (parse source) in
                  assert (printed = expected);
                  assert (format ~width (parse printed) = expected))
                [""; "\n"])
            [
              ("// note", "    {/* note */}");
              ("/* note */", "    {/* note */}");
              ("/* a\n * b */", "    {\n      /* a\n       * b */\n    }");
              ("{// note\n}", "    {/* note */}");
              ("{\n// a\n// b\n}", "    {\n      /* a */\n      /* b */\n    }");
              ("{// a\n}\n{// b\n}", "    {/* a */}\n    {/* b */}");
            ])
        [("<div>", "</div>"); ("<>", "</>")])
    [20; 40; 80; 100; 120];
  List.iter
    (fun (source, expected) ->
      let original = parse source in
      let styles =
        List.map Res_comment.is_single_line_comment original.comments
      in
      let printed = format ~width:80 original in
      assert (printed = expected);
      assert (
        List.map Res_comment.is_single_line_comment original.comments = styles);
      assert (format ~width:80 (parse printed) = expected))
    [
      ("let x = <>// empty\n</>", "let x = <>{/* empty */}</>\n");
      ( "let x = <>/* a */ // b\n/* c */</>",
        "let x =\n\
        \  <>\n\
        \    {\n\
        \      /* a */\n\
        \      /* b */\n\
        \      /* c */\n\
        \    }\n\
        \  </>\n" );
      ( "let x = <span> hello // note\n</span>",
        "let x = <span>{hello /* note */}</span>\n" );
      ("let x = <> hello // note\n</>", "let x = <>{hello /* note */}</>\n");
      ( "let x = <span> hello /* block */ // note\n</span>",
        "let x = <span>{hello /* block */ /* note */}</span>\n" );
      ( "let x = <span>{hello // inside\n}</span>",
        "let x = <span>{hello /* inside */}</span>\n" );
      ( "let x = <div>{// note\n} {value}</div>",
        "let x =\n  <div>\n    {/* note */}\n    {value}\n  </div>\n" );
      ( "let x = <span> // before\nhello // contains */\n</span>",
        "let x =\n\
        \  <span>\n\
        \    {\n\
        \      /* before */\n\
        \      hello // contains */\n\
        \    }\n\
        \  </span>\n" );
      ("let x = y // outside", "let x = y // outside\n");
    ];
  List.iter
    (fun width ->
      List.iter
        (fun (opening, closing) ->
          List.iter
            (fun text ->
              let source =
                "let x = " ^ opening ^ "{//" ^ text ^ "\n}" ^ closing
              in
              let expected =
                "let x =\n  " ^ opening ^ "\n    {\n      //" ^ text
                ^ "\n    }\n  " ^ closing ^ "\n"
              in
              let printed = format ~width (parse source) in
              assert (printed = expected);
              assert (format ~width (parse printed) = expected))
            [" starts /*"; " closes */"; " /* nested */"; " */ before /*"])
        [("<div>", "</div>"); ("<>", "</>")])
    [20; 40; 80; 100; 120];
  List.iter
    (fun (opening, closing) ->
      let source =
        "let x = " ^ opening
        ^ "{value ? <XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX /> \
           : <YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY />}"
        ^ closing
      in
      let expected =
        "let x =\n  " ^ opening
        ^ "\n\
          \    {value\n\
          \      ? <XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX />\n\
          \      : <YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY />}\n\
          \  " ^ closing ^ "\n"
      in
      let printed = format ~width:80 (parse source) in
      assert (printed = expected);
      assert (format ~width:80 (parse printed) = expected))
    [("<div>", "</div>"); ("<>", "</>")];
  (* Comment containers must not alter the zero/one/many-child distinction,
     including in the React and generic JSX transforms. *)
  List.iter
    (fun (source, without_comments) ->
      let actual = parse source in
      let expected = parse without_comments in
      assert (expression_structure actual = expression_structure expected);
      List.iter
        (fun jsx_module ->
          let rewrite result =
            let parsetree =
              Jsx_ppx.rewrite_implementation ~jsx_version:4 ~jsx_module
                result.Res_driver.parsetree
            in
            expression_structure {result with parsetree}
          in
          assert (rewrite actual = rewrite expected))
        ["React"; "CustomJsx"])
    [
      ("let x = <span>{/* empty */}</span>", "let x = <span></span>");
      ("let x = <>{// empty\n}</>", "let x = <></>");
      ("let x = <C>{/* empty */}</C>", "let x = <C></C>");
      ("let x = <C>{/* before */}<A />{/* after */}</C>", "let x = <C><A /></C>");
      ("let x = <><A />{/* between */}<B /></>", "let x = <><A /><B /></>");
      ("let x = <span>{}</span>", "let x = <span>{{}}</span>");
      ("let x = <span>/* before */{}</span>", "let x = <span>{{}}</span>");
      ("let x = <span>{{/* record */}}</span>", "let x = <span>{{}}</span>");
    ];
  List.iter
    (fun source ->
      let result =
        Res_driver.parse_implementation_from_source ~display_filename:filename
          ~source
      in
      assert result.invalid)
    ["let x = <div>{/* missing brace */</div>"; "let x = <div>{/* unterminated"];
  assert (
    format ~width:80 (parse "let x = <span>/* empty */</span>")
    = "let x = <span>{/* empty */}</span>\n");
  assert (
    format ~width:80 (parse "let x = <>/* inside */</> // outside")
    = "let x = <>{/* inside */}</> // outside\n");
  print_endline "✅ JSX child migration preserves expressions and comments"

module Outcome_printer_tests = struct
  let signature_to_outcome structure =
    Lazy.force Res_outcome_printer.setup;

    (Clflags.current ()).include_dirs :=
      Filename.concat "lib" "ocaml" :: !((Clflags.current ()).include_dirs);
    Res_compmisc.init_path ();
    (Clflags.current ()).nopervasives := true;
    let env = Res_compmisc.initial_env () in
    try
      let _typedStructure, signature, _newenv =
        Typemod.type_toplevel_phrase env structure
      in
      signature |> Printtyp.tree_of_signature
      |> !Oprint.out_signature Format.str_formatter;
      Format.flush_str_formatter ()
    with
    | Typetexp.Error (_, _, err) ->
      Typetexp.report_error env Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1
    | Typemod.Error (_, _, err) ->
      Typemod.report_error env Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1
    | Typedecl.Error (_, err) ->
      Typedecl.report_error Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1
    | e ->
      prerr_string
        ("Unknown error while trying to print outcome tree.\n"
       ^ "We don't display all the outcome type errors; try adding the new \
          case to the `try` pattern match.\n");
      raise e

  (* `data/oprint/oprint.res` will be read into memory and typechecked.
   * The inferred signature (i.e. the type of the module `oprint.res`) will
   * then be converted to the outcome tree.
   * The outcome tree is printed to a string
   * and stored in a snapshot `data/oprint/expected/oprint.resi.txt` *)
  let run () =
    let filename = Filename.concat data_dir "oprint/oprint.res" in
    let result = Res_driver.parsing_engine.parse_implementation ~filename in
    let signature =
      if result.Res_driver.invalid then (
        Res_driver.parsing_engine.string_of_diagnostics ~source:result.source
          ~filename:result.filename result.diagnostics;
        exit 1)
      else result.Res_driver.parsetree
    in
    Ext_io.write_file
      (Filename.concat data_dir "oprint/expected/oprint.resi.txt")
      (signature_to_outcome signature)
end

module Parser_api_test = struct
  let make_default () =
    let src = "   let x = 1\nlet y = 2\nlet z = 3" in
    let parser = Res_parser.make src "test.res" in
    assert (Res_parser.position parser = Lexing.dummy_pos);
    assert (Res_parser.peek parser = Res_token.Let {unwrap = false});
    assert ((Res_parser.start_pos parser).pos_lnum = 1);
    assert ((Res_parser.start_pos parser).pos_bol = 0);
    assert ((Res_parser.end_pos parser).pos_cnum = 6);
    assert (Res_parser.position parser = Lexing.dummy_pos);
    Res_parser.next parser;
    assert ((Res_parser.position parser).pos_cnum = 6);
    print_endline "✅ Parser make: initializes parser and checks positions"

  let unix_lf () =
    let src = "let x = 1\nlet y = 2\nlet z = 3" in
    let parser = Res_parser.make src "test.res" in
    (match Res_core.parse_implementation parser with
    | [x; y; z] ->
      assert (x.pstr_loc.loc_start.pos_lnum = 1);
      assert (y.pstr_loc.loc_start.pos_lnum = 2);
      assert (z.pstr_loc.loc_start.pos_lnum = 3)
    | _ -> assert false);
    print_endline "✅ Parser handles LF correct"

  let windows_crlf () =
    let src = "let x = 1\r\nlet y = 2\r\nlet z = 3" in
    let parser = Res_parser.make src "test.res" in
    (match Res_core.parse_implementation parser with
    | [x; y; z] ->
      assert (x.pstr_loc.loc_start.pos_lnum = 1);
      assert (y.pstr_loc.loc_start.pos_lnum = 2);
      assert (z.pstr_loc.loc_start.pos_lnum = 3)
    | _ -> assert false);
    print_endline "✅ Parser handles CRLF correct"

  let run () =
    make_default ();
    unix_lf ();
    windows_crlf ()
end

let () = Outcome_printer_tests.run ()
let () = Parser_api_test.run ()
let () = Res_utf8_test.run ()
