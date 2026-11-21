type ('ast, 'diagnostics) parse_result = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  warnings: Res_warning.t list;
  invalid: bool;
  comments: Res_comment.t list;
  newline: string;
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
}

type print_engine = {
  print_implementation:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.structure ->
    unit;
  print_interface:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.signature ->
    unit;
}

let setup ~load_file ~filename =
  let src = load_file filename in
  Res_parser.make src filename

let setup_from_source ~display_filename ~source =
  Res_parser.make source display_filename

let make_parsing_engine ~load_file =
  {
    parse_implementation =
      (fun ~filename ->
        let engine = setup ~load_file ~filename in
        let structure = Res_core.parse_implementation engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.filename;
          source = engine.source;
          parsetree = structure;
          diagnostics;
          warnings = List.rev engine.warnings;
          invalid;
          comments = List.rev engine.comments;
          newline = Res_parser.newline engine;
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
          filename = engine.filename;
          source = engine.source;
          parsetree = structure;
          diagnostics;
          warnings = List.rev engine.warnings;
          invalid;
          comments = List.rev engine.comments;
          newline = Res_parser.newline engine;
        });
    parse_interface =
      (fun ~filename ->
        let engine = setup ~load_file ~filename in
        let signature = Res_core.parse_specification engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.filename;
          source = engine.source;
          parsetree = signature;
          diagnostics;
          warnings = List.rev engine.warnings;
          invalid;
          comments = List.rev engine.comments;
          newline = Res_parser.newline engine;
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
          filename = engine.filename;
          source = engine.source;
          parsetree = signature;
          diagnostics;
          warnings = List.rev engine.warnings;
          invalid;
          comments = List.rev engine.comments;
          newline = Res_parser.newline engine;
        });
  }

let parsing_engine =
  make_parsing_engine ~load_file:(fun filename ->
      In_channel.with_open_bin filename In_channel.input_all)

let parse_implementation_from_source ~display_filename ~source =
  let engine = setup_from_source ~display_filename ~source in
  let structure = Res_core.parse_implementation engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in
  {
    filename = engine.filename;
    source = engine.source;
    parsetree = structure;
    diagnostics;
    warnings = List.rev engine.warnings;
    invalid;
    comments = List.rev engine.comments;
    newline = Res_parser.newline engine;
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
    filename = engine.filename;
    source = engine.source;
    parsetree = signature;
    diagnostics;
    warnings = List.rev engine.warnings;
    invalid;
    comments = List.rev engine.comments;
    newline = Res_parser.newline engine;
  }
