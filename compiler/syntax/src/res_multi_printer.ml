(* print res files to res syntax *)
let print_res ~ignore_parse_errors ~is_interface ~filename =
  if is_interface then (
    let parse_result = Res_driver.parsing_engine.parse_interface ~filename in
    if parse_result.invalid then (
      Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then raise_notrace Res_driver.Already_reported);
    Res_printer.print_interface ~width:Res_printer.default_print_width
      ~comments:parse_result.comments parse_result.parsetree)
  else
    let parse_result =
      Res_driver.parsing_engine.parse_implementation ~filename
    in
    if parse_result.invalid then (
      Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then raise_notrace Res_driver.Already_reported);
    Res_printer.print_implementation ~width:Res_printer.default_print_width
      ~comments:parse_result.comments parse_result.parsetree
[@@raises Res_driver.Already_reported]

(* print the given file named input to from "language" to res, general interface exposed by the compiler *)
let print ?(ignore_parse_errors = false) input =
  let is_interface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  print_res ~ignore_parse_errors ~is_interface ~filename:input
[@@raises Res_driver.Already_reported]

(* suppress unused optional arg *)
let _ = fun s -> print ~ignore_parse_errors:false s
[@@raises Res_driver.Already_reported]
