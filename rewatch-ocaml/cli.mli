type command =
  | Build of build_options
  | Clean of {verbosity: int; folder: string; prod: bool}
  | Watch of build_options
  | Format of format_input
  | Compiler_args of string

and build_options = {
  verbosity: int;
  folder: string;
  prod: bool;
  features: string list option;
  warn_error: string option;
  after_build: string option;
  filter: Source_filter.t option;
  clear_screen: bool;
  no_timing: bool;
}

and format_input =
  | Format_stdin of string
  | Format_files of {check: bool; paths: string list}

type evaluation = Run of command | Exit of int

exception Parse_error of string
exception Help
exception Version

val eval : string array -> evaluation
val parse : string array -> command
