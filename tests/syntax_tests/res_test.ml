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
  let filename = Filename.concat data_dir "printer/expr/jsxChildren.res" in
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
        (fun (source, expected) ->
          let printed = format ~width (parse source) in
          assert (printed = expected);
          assert (format ~width (parse printed) = expected))
        [
          ( "let x = <>// empty\n</>",
            "let x =\n  <>\n    {\n      // empty\n    }\n  </>\n" );
          ( "let x = <>/* a */ // b\n/* c */</>",
            "let x =\n\
            \  <>\n\
            \    {\n\
            \      /* a */\n\
            \      // b\n\
            \      /* c */\n\
            \    }\n\
            \  </>\n" );
          ( "let x = <span> hello // note\n</span>",
            "let x =\n  <span>\n    {\n      hello // note\n    }\n  </span>\n"
          );
          ( "let x = <> hello // note\n</>",
            "let x =\n  <>\n    {\n      hello // note\n    }\n  </>\n" );
          ( "let x = <span> hello /* block */ // note\n</span>",
            "let x =\n\
            \  <span>\n\
            \    {\n\
            \      hello /* block */ // note\n\
            \    }\n\
            \  </span>\n" );
          ( "let x = <span> // before\nhello // contains */\n</span>",
            "let x =\n\
            \  <span>\n\
            \    {\n\
            \      // before\n\
            \      hello // contains */\n\
            \    }\n\
            \  </span>\n" );
        ])
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

    Clflags.include_dirs :=
      Filename.concat "lib" "ocaml" :: !Clflags.include_dirs;
    Res_compmisc.init_path ();
    Clflags.nopervasives := true;
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
