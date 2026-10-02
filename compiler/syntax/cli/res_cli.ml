(*
  This CLI isn't used apart for this repo's testing purposes. The syntax
  itself is used by ReScript's compiler programmatically through various other apis.
*)

(* command line flags *)
module Res_clflags : sig
  val recover : bool ref
  val print : string ref
  val width : int ref
  val file : string ref
  val interface : bool ref
  val jsx_version : int ref
  val jsx_module : string ref
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
  let test_ast_conversion = ref false

  let usage =
    "\n\
     **This command line is for the repo developer's testing purpose only. DO \
     NOT use it in production**!\n\n"
    ^ "Usage:\n  res_parser <options> <file>\n\n" ^ "Examples:\n"
    ^ "  res_parser myFile.res\n" ^ "  res_parser -print ml myFile.res\n"
    ^ "  res_parser -print binary -interface myFile.resi\n\n" ^ "Options are:"

  let spec =
    [
      ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
      ( "-print",
        Arg.String (fun txt -> print := txt),
        "Print either binary, ml, ast, sexp, comments, tokens or res. Default: \
         res" );
      ( "-width",
        Arg.Int (fun w -> width := w),
        "Specify the line length for the printer (formatter)" );
      ( "-interface",
        Arg.Unit (fun () -> interface := true),
        "Parse as interface" );
      ( "-jsx-version",
        Arg.Int (fun i -> jsx_version := i),
        "Apply the built-in JSX transform before printing: 4. Default: none" );
      ( "-jsx-module",
        Arg.String (fun txt -> jsx_module := txt),
        "Specify the jsx module. Default: react" );
      ( "-test-ast-conversion",
        Arg.Unit (fun () -> test_ast_conversion := true),
        "Test the ast conversion" );
    ]

  let parse () = Arg.parse spec (fun f -> file := f) usage
end

module Cli_arg_processor = struct
  type backend = Parser : 'diagnostics Res_driver.parsing_engine -> backend
  [@@unboxed]

  let process_file ~is_interface ~width ~recover ~target ~jsx_version
      ~jsx_module ~test_ast_conversion filename =
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
      | "tokens" -> Res_token_debugger.token_print_engine
      | "res" -> Res_driver.print_engine
      | target ->
        print_endline
          ("-print needs to be either binary, ml, ast, sexp, comments, tokens \
            or res. You provided " ^ target);
        exit 1
    in

    let (Parser backend) = parsing_engine in
    (* Color the Format tags (e.g. @{<error>...@}) of printed diagnostics *)
    Misc.Color.setup None;

    (* Special case for tokens - bypass parsing entirely *)
    if target = "tokens" then
      print_engine.print_implementation ~width ~filename ~comments:[] []
    else if process_interface then
      let parse_result = backend.parse_interface ~filename in
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
      let parse_result = backend.parse_implementation ~filename in
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
    Res_clflags.parse ();
    Cli_arg_processor.process_file ~is_interface:!Res_clflags.interface
      ~width:!Res_clflags.width ~recover:!Res_clflags.recover
      ~target:!Res_clflags.print ~jsx_version:!Res_clflags.jsx_version
      ~jsx_module:!Res_clflags.jsx_module !Res_clflags.file
      ~test_ast_conversion:!Res_clflags.test_ast_conversion)
[@@raises exit]
