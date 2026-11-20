type ('ast, 'diagnostics) parse_result = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Res_comment.t list;
  newline: string;
      (* The newline to format the source with: CRLF when its first line break
         outside literals and comments is CRLF, LF otherwise. The printer
         copies literals and comments with their bytes unchanged. *)
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

val parse_implementation_from_source :
  display_filename:string ->
  source:string ->
  (Parsetree.structure, Res_diagnostics.t list) parse_result
[@@live]

val parse_interface_from_source :
  display_filename:string ->
  source:string ->
  (Parsetree.signature, Res_diagnostics.t list) parse_result
[@@live]

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

val make_parsing_engine :
  load_file:(string -> string) -> Res_diagnostics.t list parsing_engine
(** A parsing engine that reads files with [load_file]. *)

val parsing_engine : Res_diagnostics.t list parsing_engine
(** A parsing engine that reads files from disk. *)
