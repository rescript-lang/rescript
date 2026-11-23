(* print res files to res syntax *)
let print_res ~ignore_parse_errors ~is_interface ~filename source =
  if is_interface then (
    let parse_result =
      Res_driver.parse_interface_from_source ~display_filename:filename ~source
    in
    Syntax_driver.report_warnings parse_result.warnings;
    if parse_result.invalid then (
      Syntax_driver.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then
        raise_notrace Syntax_driver.Already_reported);
    Res_printer.print_interface ~width:Res_printer.default_print_width
      ~newline:parse_result.newline ~comments:parse_result.comments
      parse_result.parsetree)
  else
    let parse_result =
      Res_driver.parse_implementation_from_source ~display_filename:filename
        ~source
    in
    Syntax_driver.report_warnings parse_result.warnings;
    if parse_result.invalid then (
      Syntax_driver.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then
        raise_notrace Syntax_driver.Already_reported);
    Res_printer.print_implementation ~width:Res_printer.default_print_width
      ~newline:parse_result.newline ~comments:parse_result.comments
      parse_result.parsetree
[@@throws Syntax_driver.Already_reported]

(* print the given file named input to from "language" to res, general interface exposed by the compiler *)
let print ?(ignore_parse_errors = false) input =
  let is_interface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  let source = File_io.load_file input in
  (* Keep a byte order mark, which the scanner skips *)
  Res_scanner.byte_order_mark source
  ^ print_res ~ignore_parse_errors ~is_interface ~filename:input source
[@@throws Syntax_driver.Already_reported]

(* suppress unused optional arg *)
let _ = fun s -> print ~ignore_parse_errors:false s
[@@throws Syntax_driver.Already_reported]
