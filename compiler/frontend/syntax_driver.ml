(* The compiler's side of parsing: reading sources through the build's file
   store, reporting syntax errors, and the entry points of the compiler. *)

let print_report ?(custom_intro = None)
    ?(formatter = Compiler_request_output.stderr_formatter ()) diagnostics src =
  let rec print diagnostics =
    match diagnostics with
    | [] -> ()
    | d :: rest ->
      Loc_report.report_error ~custom_intro ~src:(Some src) formatter
        {
          loc =
            {
              loc_start = Res_diagnostics.get_start_pos d;
              loc_end = Res_diagnostics.get_end_pos d;
              loc_ghost = false;
            };
          msg = Res_diagnostics.explain_in_source ~src d;
          sub = [];
          if_highlight = "";
        };
      (match rest with
      | [] -> ()
      | _ -> Format.fprintf formatter "@.");
      print rest
  in
  Format.fprintf formatter "@[<v>";
  print (List.rev diagnostics);
  Format.fprintf formatter "@]@."

let report_warnings warnings =
  List.iter
    (fun ({loc; kind} : Res_warning.t) ->
      Loc_report.prerr_warning loc
        (Warnings.Deprecated (Res_warning.message kind, loc, loc, false)))
    warnings

let parsing_engine = Res_driver.make_parsing_engine ~load_file:File_io.load_file

exception Already_reported

let parse_implementation ~ignore_parse_errors sourcefile =
  Loc_report.set_input_name sourcefile;
  let parse_result = parsing_engine.parse_implementation ~filename:sourcefile in
  report_warnings parse_result.warnings;
  if parse_result.invalid then (
    print_report parse_result.diagnostics parse_result.source;
    if not ignore_parse_errors then raise_notrace Already_reported);
  parse_result.parsetree
[@@throws Already_reported]

let parse_interface ~ignore_parse_errors sourcefile =
  Loc_report.set_input_name sourcefile;
  let parse_result = parsing_engine.parse_interface ~filename:sourcefile in
  report_warnings parse_result.warnings;
  if parse_result.invalid then (
    print_report parse_result.diagnostics parse_result.source;
    if not ignore_parse_errors then raise_notrace Already_reported);
  parse_result.parsetree
[@@throws Already_reported]
