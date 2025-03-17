(*
  This CLI isn't used apart for this repo's testing purposes. The syntax
  itself is used by ReScript's compiler programmatically through various other apis.
*)

(*
  This is OCaml's Misc.ml's Color module. More specifically, this is
  ReScript's OCaml fork's Misc.ml's Color module:
  https://github.com/rescript-lang/ocaml/blob/92e58bedced8d7e3e177677800a38922327ab860/utils/misc.ml#L540

  The syntax's printing's coloring logic depends on:
    1. a global mutable variable that's set in the compiler: Misc.Color.color_enabled
    2. the colors tags supported by Misc.Color, e.g. style_of_tag, which Format
      tags like @{<error>hello@} use
    3. etc.

  When this syntax is programmatically used inside ReScript, the various
  Format tags like <error> and <dim> get properly colored depending on the
  above points.

  But when used by this cli file, that coloring logic doesn't render properly
  because we're compiling against vanilla OCaml 4.06 instead of ReScript's
  OCaml fork. For example, the vanilla compiler doesn't support the `dim`
  color (grey). So we emulate the right coloring logic by copy pasting how our
  forked OCaml compiler does it.
*)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black [@live]
    | Red
    | Green [@live]
    | Yellow
    | Blue [@live]
    | Magenta
    | Cyan
    | White [@live]

  type style =
    | FG of color (* foreground *)
    | BG of color [@live] (* background *)
    | Bold
    | Reset
    | Dim

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"
    | Dim -> "2"

  let ansi_of_style_l l =
    let s =
      match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {error: style list; warning: style list; loc: style list}

  let default_styles =
    {warning = [Bold; FG Magenta]; error = [Bold; FG Red]; loc = [Bold]}

  let cur_styles = ref default_styles

  (* let get_styles () = !cur_styles *)
  (* let set_styles s = cur_styles := s *)

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s =
    match s with
    | Format.String_tag "error" -> !cur_styles.error
    | Format.String_tag "warning" -> !cur_styles.warning
    | Format.String_tag "loc" -> !cur_styles.loc
    | Format.String_tag "info" -> [Bold; FG Yellow]
    | Format.String_tag "dim" -> [Dim]
    | Format.String_tag "filename" -> [FG Cyan]
    | _ -> raise Not_found
  [@@raises Not_found]

  let color_enabled = ref true

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' =
      {
        functions with
        mark_open_stag = mark_open_tag ~or_else:functions.mark_open_stag;
        mark_close_stag = mark_close_tag ~or_else:functions.mark_close_stag;
      }
    in
    pp_set_mark_tags ppf true;
    (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    (* also setup margins *)
    pp_set_margin ppf (pp_get_margin std_formatter ());
    ()

  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb" && term <> "" && isatty stderr

  type setting = Auto [@live] | Always [@live] | Never [@live]

  let setup =
    let first = ref true in
    (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled :=
          match o with
          | Some Always -> true
          | Some Auto -> should_enable_color ()
          | Some Never -> false
          | None -> should_enable_color ());
      ()
end

(* command line flags *)
module ResClflags : sig
  val recover : bool ref
  val print : string ref
  val width : int ref
  val file : string ref
  val interface : bool ref
  val jsx_version : int ref
  val jsx_module : string ref
  val typechecker : bool ref
  val test_ast_conversion : bool ref

  val parse : unit -> unit
end = struct
  let recover = ref false
  let width = ref 100

  let print = ref "res"
  let interface = ref false
  let jsx_version = ref (-1)
  let jsx_module = ref "react"
  let file = ref ""
  let typechecker = ref false
  let test_ast_conversion = ref false

  let usage =
    "\n\
     **This command line is for the repo developer's testing purpose only. DO \
     NOT use it in production**!\n\n"
    ^ "Usage:\n  res_parser <options> <file>\n\n" ^ "Examples:\n"
    ^ "  res_parser myFile.res\n"
    ^ "  res_parser -parse ml -print res myFile.ml\n"
    ^ "  res_parser -parse res -print binary -interface myFile.resi\n\n"
    ^ "Options are:"

  let spec =
    [
      ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
      ( "-print",
        Arg.String (fun txt -> print := txt),
        "Print either binary, ml, ast, sexp, comments or res. Default: res" );
      ( "-width",
        Arg.Int (fun w -> width := w),
        "Specify the line length for the printer (formatter)" );
      ( "-interface",
        Arg.Unit (fun () -> interface := true),
        "Parse as interface" );
      ( "-jsx-version",
        Arg.Int (fun i -> jsx_version := i),
        "Apply a specific built-in ppx before parsing, none or 3, 4. Default: \
         none" );
      ( "-jsx-module",
        Arg.String (fun txt -> jsx_module := txt),
        "Specify the jsx module. Default: react" );
      ( "-typechecker",
        Arg.Unit (fun () -> typechecker := true),
        "Parses the ast as it would be passed to the typechecker and not the \
         printer" );
      ( "-test-ast-conversion",
        Arg.Unit (fun () -> test_ast_conversion := true),
        "Test the ast conversion" );
    ]

  let parse () = Arg.parse spec (fun f -> file := f) usage
end

module CliArgProcessor = struct
  type backend = Parser : 'diagnostics Res_driver.parsing_engine -> backend
  [@@unboxed]

  let process_file ~is_interface ~width ~recover ~target ~jsx_version
      ~jsx_module ~typechecker ~test_ast_conversion filename =
    let len = String.length filename in
    let process_interface =
      is_interface
      || (len > 0 && (String.get [@doesNotRaise]) filename (len - 1) = 'i')
    in
    let parsing_engine = Parser Res_driver.parsing_engine in
    let print_engine =
      match target with
      | "binary" -> Res_driver_binary.print_engine
      | "ml" -> Res_driver_ml_printer.print_engine
      | "ast" -> Res_ast_debugger.print_engine
      | "sexp" -> Res_ast_debugger.sexp_print_engine
      | "comments" -> Res_ast_debugger.comments_print_engine
      | "res" -> Res_driver.print_engine
      | target ->
        print_endline
          ("-print needs to be either binary, ml, ast, sexp, comments or res. \
            You provided " ^ target);
        exit 1
    in

    let for_printer =
      match target with
      | ("res" | "sexp") when not typechecker -> true
      | _ -> false
    in

    let (Parser backend) = parsing_engine in
    (* This is the whole purpose of the Color module above *)
    Color.setup None;
    if process_interface then
      let parse_result = backend.parse_interface ~for_printer ~filename in
      if parse_result.invalid then (
        backend.string_of_diagnostics ~source:parse_result.source
          ~filename:parse_result.filename parse_result.diagnostics;
        if recover then
          print_engine.print_interface ~width ~filename
            ~comments:parse_result.comments parse_result.parsetree
        else exit 1)
      else
        let parsetree =
          if not test_ast_conversion then parse_result.parsetree
          else
            let tree0 =
              Ast_mapper_to0.default_mapper.signature
                Ast_mapper_to0.default_mapper parse_result.parsetree
            in
            Ast_mapper_from0.default_mapper.signature
              Ast_mapper_from0.default_mapper tree0
        in
        let parsetree =
          Jsx_ppx.rewrite_signature ~jsx_version ~jsx_module parsetree
        in
        print_engine.print_interface ~width ~filename
          ~comments:parse_result.comments parsetree
    else
      let parse_result = backend.parse_implementation ~for_printer ~filename in
      if parse_result.invalid then (
        backend.string_of_diagnostics ~source:parse_result.source
          ~filename:parse_result.filename parse_result.diagnostics;
        if recover then
          print_engine.print_implementation ~width ~filename
            ~comments:parse_result.comments parse_result.parsetree
        else exit 1)
      else
        let parsetree =
          if not test_ast_conversion then parse_result.parsetree
          else
            let tree0 =
              Ast_mapper_to0.default_mapper.structure
                Ast_mapper_to0.default_mapper parse_result.parsetree
            in
            Ast_mapper_from0.default_mapper.structure
              Ast_mapper_from0.default_mapper tree0
        in
        let parsetree =
          Jsx_ppx.rewrite_implementation ~jsx_version ~jsx_module parsetree
        in
        print_engine.print_implementation ~width ~filename
          ~comments:parse_result.comments parsetree
  [@@raises exit]
end

let () =
  if not !Sys.interactive then (
    ResClflags.parse ();
    CliArgProcessor.process_file ~is_interface:!ResClflags.interface
      ~width:!ResClflags.width ~recover:!ResClflags.recover
      ~target:!ResClflags.print ~jsx_version:!ResClflags.jsx_version
      ~jsx_module:!ResClflags.jsx_module ~typechecker:!ResClflags.typechecker
      !ResClflags.file
      ~test_ast_conversion:!ResClflags.test_ast_conversion)
[@@raises exit]
