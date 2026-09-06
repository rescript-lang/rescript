module IO = Res_io

type ('ast, 'diagnostics) parse_result = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Res_comment.t list;
}

type 'diagnostics parsing_engine = {
  parse_implementation:
    filename:string -> (Parsetree.structure, 'diagnostics) parse_result;
  parse_implementation_from_source:
    source:string -> (Parsetree.structure, 'diagnostics) parse_result;
  parse_interface:
    filename:string -> (Parsetree.signature, 'diagnostics) parse_result;
  parse_interface_from_source:
    source:string -> (Parsetree.signature, 'diagnostics) parse_result;
  string_of_diagnostics:
    source:string -> filename:string -> 'diagnostics -> unit;
}

type print_engine = {
  print_implementation:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.structure ->
    unit;
  print_implementation_from_source:
    width:int ->
    source:string ->
    comments:Res_comment.t list ->
    Parsetree.structure ->
    unit;
  print_interface:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.signature ->
    unit;
  print_interface_from_source:
    width:int ->
    source:string ->
    comments:Res_comment.t list ->
    Parsetree.signature ->
    unit;
}

let setup ~filename =
  let src = IO.read_file ~filename in
  Res_parser.make src filename

let setup_from_source ~display_filename ~source =
  Res_parser.make source display_filename

let parsing_engine =
  {
    parse_implementation =
      (fun ~filename ->
        let engine = setup ~filename in
        let structure = Res_core.parse_implementation engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = structure;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    parse_implementation_from_source =
      (fun ~source ->
        let engine = setup_from_source ~source ~display_filename:"source" in
        let structure = Res_core.parse_implementation engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = structure;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    parse_interface =
      (fun ~filename ->
        let engine = setup ~filename in
        let signature = Res_core.parse_specification engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = signature;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    parse_interface_from_source =
      (fun ~source ->
        let engine = setup_from_source ~source ~display_filename:"<source>" in
        let signature = Res_core.parse_specification engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = signature;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    string_of_diagnostics =
      (fun ~source ~filename:_ diagnostics ->
        Res_diagnostics.print_report diagnostics source);
  }

let parse_implementation_from_source ~display_filename ~source =
  let engine = setup_from_source ~display_filename ~source in
  let structure = Res_core.parse_implementation engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in
  {
    filename = engine.scanner.filename;
    source = engine.scanner.src;
    parsetree = structure;
    diagnostics;
    invalid;
    comments = List.rev engine.comments;
  }

let parse_interface_from_source ~display_filename ~source =
  let engine = setup_from_source ~display_filename ~source in
  let signature = Res_core.parse_specification engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in
  {
    filename = engine.scanner.filename;
    source = engine.scanner.src;
    parsetree = signature;
    diagnostics;
    invalid;
    comments = List.rev engine.comments;
  }

let print_engine =
  {
    print_implementation =
      (fun ~width ~filename:_ ~comments structure ->
        print_string
          (Res_printer.print_implementation ~width structure ~comments));
    print_implementation_from_source =
      (fun ~width ~source:_ ~comments structure ->
        print_string
          (Res_printer.print_implementation ~width structure ~comments));
    print_interface =
      (fun ~width ~filename:_ ~comments signature ->
        print_string (Res_printer.print_interface ~width signature ~comments));
    print_interface_from_source =
      (fun ~width ~source:_ ~comments signature ->
        print_string (Res_printer.print_interface ~width signature ~comments));
  }

let parse_implementation ?(ignore_parse_errors = false) sourcefile =
  Location.input_name := sourcefile;
  let parse_result = parsing_engine.parse_implementation ~filename:sourcefile in
  if parse_result.invalid then (
    Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
    if not ignore_parse_errors then exit 1);
  parse_result.parsetree
[@@raises exit]

let parse_interface ?(ignore_parse_errors = false) sourcefile =
  Location.input_name := sourcefile;
  let parse_result = parsing_engine.parse_interface ~filename:sourcefile in
  if parse_result.invalid then (
    Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
    if not ignore_parse_errors then exit 1);
  parse_result.parsetree
[@@raises exit]

(* suppress unused optional arg *)
let _ =
 fun s ->
  ( parse_implementation ~ignore_parse_errors:false s,
    parse_interface ~ignore_parse_errors:false s )
[@@raises exit]
