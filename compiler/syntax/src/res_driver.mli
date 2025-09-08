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
    for_printer:bool ->
    filename:string ->
    (Parsetree.structure, 'diagnostics) parse_result;
  parse_interface:
    for_printer:bool ->
    filename:string ->
    (Parsetree.signature, 'diagnostics) parse_result;
  string_of_diagnostics:
    source:string -> filename:string -> 'diagnostics -> unit;
}

val parse_implementation_from_source :
  for_printer:bool ->
  display_filename:string ->
  source:string ->
  (Parsetree.structure, Res_diagnostics.t list) parse_result
[@@live]

val parse_interface_from_source :
  for_printer:bool ->
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

val parsing_engine : Res_diagnostics.t list parsing_engine

val print_engine : print_engine

(* ReScript implementation parsing compatible with ocaml pparse driver. Used by the compiler. *)
val parse_implementation :
  ?ignore_parse_errors:bool -> string -> Parsetree.structure
[@@live] [@@raises Location.Error]

(* ReScript interface parsing compatible with ocaml pparse driver. Used by the compiler *)
val parse_interface : ?ignore_parse_errors:bool -> string -> Parsetree.signature
[@@live] [@@raises Location.Error]
