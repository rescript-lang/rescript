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

let () = print_endline "✅ multi printer api tests"

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

(* Res_doc.item_separator puts a blank line next to an item that prints on
   several lines, measuring the documents on each side as to_string lays them
   out. Res_doc.comment hides comments from the measurement. *)
module Multiline_item_spacing_test = struct
  module Doc = Res_doc

  let check name ~expected actual =
    if actual <> expected then
      failwith
        (Printf.sprintf "%s:\nexpected:\n%S\nactual:\n%S" name expected actual)

  (* Module items are printed inside a broken group, like print_structure. *)
  let print ?(width = 80) doc =
    Doc.to_string ~width (Doc.breakable_group ~force_break:true doc)
  let items prev next = Doc.concat [prev; Doc.item_separator ~prev ~next; next]
  let a = Doc.text "let a = 1"
  let b = Doc.text "let b = 2"

  (* A function call that fits on one line at width 80 but not at width 10. *)
  let call =
    Doc.group
      (Doc.concat
         [
           Doc.text "f(";
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.text "x,";
                  Doc.line;
                  Doc.text "y";
                  Doc.trailing_comma;
                ]);
           Doc.soft_line;
           Doc.text ")";
         ])

  let measuring () =
    check "one-line items stay together" ~expected:"let a = 1\nlet b = 2"
      (print (items a b));
    check "a hard line spans lines" ~expected:"x\ny\n\nlet b = 2"
      (print (items (Doc.concat [Doc.text "x"; Doc.hard_line; Doc.text "y"]) b));
    check "a literal line spans lines" ~expected:"`x\ny`\n\nlet b = 2"
      (print
         (items (Doc.concat [Doc.text "`x"; Doc.literal_line; Doc.text "y`"]) b));
    check "a newline inside text spans lines" ~expected:"`x\ny`\n\nlet b = 2"
      (print (items (Doc.text "`x\ny`") b));
    check "an item list spans lines"
      ~expected:"let a = 1\nlet b = 2\n\nlet c = 3"
      (print (items (items a b) (Doc.text "let c = 3")));
    check "a group that fits is one line" ~expected:"f(x, y)\nlet b = 2"
      (print (items call b));
    check "a group that breaks spans lines"
      ~expected:"f(\n  x,\n  y,\n)\n\nlet b = 2"
      (print ~width:5 (items call b));
    check "a chosen custom layout is measured" ~expected:"let a = 1\nlet b = 2"
      (print (items (Doc.custom_layout [a]) b));
    check "break parents and line suffixes do not span lines"
      ~expected:"let a = 1 // c\nlet b = 2"
      (print
         (items
            (Doc.concat
               [a; Doc.break_parent; Doc.line_suffix (Doc.text " // c")])
            b));
    check "a comment above an item does not count"
      ~expected:"let a = 1\n// c\nlet b = 2"
      (print
         (items a
            (Doc.concat
               [Doc.comment (Doc.concat [Doc.text "// c"; Doc.hard_line]); b])));
    let forced_if_breaks =
      Doc.if_breaks (Doc.text "a") (Doc.concat [Doc.text "b"; Doc.hard_line])
    in
    check "an if-breaks broken by its flat branch takes the break branch"
      ~expected:"xa\nlet b = 2"
      (print (items (Doc.concat [Doc.text "x"; forced_if_breaks]) b));
    check "an if-breaks in a broken group takes the break branch"
      ~expected:"{\n}\n\nlet b = 2"
      (print
         (items
            (Doc.breakable_group ~force_break:true
               (Doc.concat
                  [
                    Doc.if_breaks (Doc.text "{") (Doc.text "(");
                    Doc.line;
                    Doc.text "}";
                  ]))
            b))

  let trailing_comments () =
    let block =
      Doc.concat [Doc.text "let a = {"; Doc.hard_line; Doc.text "}"]
    in
    let after_block suffix =
      items (Doc.concat [block; Doc.line_suffix suffix]) b
    in
    check "a same-line trailing comment stays on its line"
      ~expected:"let a = {\n} // c\n\nlet b = 2"
      (print (after_block (Doc.text " // c")));
    check "an own-line trailing comment moves below the blank line"
      ~expected:"let a = {\n}\n\n// c\nlet b = 2"
      (print (after_block (Doc.concat [Doc.hard_line; Doc.text "// c"])));
    check "an empty own-line trailing comment prints an empty line"
      ~expected:"let a = {\n}\n\n\nlet b = 2"
      (print (after_block Doc.hard_line))

  let predicates () =
    let separator = Doc.item_separator ~prev:a ~next:b in
    assert (Doc.will_break separator);
    assert (Doc.will_break Doc.break_parent);
    assert (Doc.will_break (Doc.comment Doc.hard_line));
    assert (not (Doc.will_break (Doc.comment a)));
    assert (not (Doc.is_nil separator));
    assert (not (Doc.is_nil (Doc.comment a)));
    check "an empty custom layout prints nothing" ~expected:""
      (print (Doc.custom_layout []))

  (* debug prints to stdout; send it to the null device. *)
  let debug () =
    flush stdout;
    let saved = Unix.dup Unix.stdout in
    let null = Unix.openfile Filename.null [Unix.O_WRONLY] 0 in
    Unix.dup2 null Unix.stdout;
    Fun.protect
      ~finally:(fun () ->
        flush stdout;
        Unix.dup2 saved Unix.stdout;
        Unix.close saved;
        Unix.close null)
      (fun () -> Doc.debug (Doc.concat [items a b; Doc.comment a]))

  let printer () =
    let source = "module A = {\n  let x = 1\n}\nlet y = 2\n" in
    let format () =
      let result =
        Res_driver.parse_implementation_from_source ~display_filename:"A.res"
          ~source
      in
      Res_printer.print_implementation ~width:80 result.parsetree
        ~comments:result.comments
    in
    check "the printer keeps items together by default" ~expected:source
      (format ());
    Experimental_features.enable_from_string
      (Experimental_features.to_string
         Experimental_features.MultilineItemSpacing);
    Fun.protect ~finally:Experimental_features.reset (fun () ->
        check "the printer spaces items with MultilineItemSpacing"
          ~expected:"module A = {\n  let x = 1\n}\n\nlet y = 2\n" (format ()))

  let run () =
    measuring ();
    trailing_comments ();
    predicates ();
    debug ();
    printer ();
    print_endline "✅ multi-line item spacing"
end

let () = Outcome_printer_tests.run ()
let () = Parser_api_test.run ()
let () = Res_utf8_test.run ()
let () = Multiline_item_spacing_test.run ()
