open OUnit2

let check condition message = assert_bool message condition

let environment values name = List.assoc_opt name values

let tests =
  "output_tests" >:: fun _context ->
  check
    (Output.strip_sgr
       "Usage: \027[01mrescript build\027[m [\027[04mOPTION\027[m]\n\
        rescript: \027[31munknown\027[m option"
    = "Usage: rescript build [OPTION]\nrescript: unknown option")
    "SGR styling is removed from captured diagnostics";
  check
    (Output.strip_sgr "\027[2K\rkeep \027[ unterminated \027"
    = "\027[2K\rkeep \027[ unterminated \027")
    "other escapes and incomplete sequences are kept";
  check
    (Output.compiler_cleanup_message ~color:false ~step:"1/3"
    = Printf.sprintf
        "\027[2K\r[1/3] %sCleaned previous build due to compiler update"
        Platform.clean_symbol)
    "interactive compiler cleanup format";
  check
    (Output.parsing_message ~color:true ~step:"2/3" ~count:4 ~seconds:1.5
    = Printf.sprintf
        "\027[2K\r\027[1m\027[2m[2/3]\027[0m %sParsed 4 source files in 1.50s"
        Platform.parse_symbol)
    "interactive parsing phase styles the step when color is enabled";
  check
    (Output.should_clear_screen ~clear_screen:true ~show_progress:true
       ~interactive:true)
    "interactive clear-screen";
  check
    (not
       (Output.should_clear_screen ~clear_screen:true ~show_progress:true
          ~interactive:false))
    "non-interactive clear-screen";
  check
    (not
       (Output.should_clear_screen ~clear_screen:false ~show_progress:true
          ~interactive:true))
    "disabled clear-screen";
  check
    (not
       (Output.should_clear_screen ~clear_screen:true ~show_progress:false
          ~interactive:true))
    "quiet watch mode should preserve the terminal";
  check
    (Output.colors_enabled_with
       ~getenv:(environment [("CLICOLOR_FORCE", "1")])
       ~win32:false ~interactive:false)
    "CLICOLOR_FORCE enables redirected colors";
  check
    (not
       (Output.colors_enabled_with
          ~getenv:(environment [("CLICOLOR_FORCE", "0"); ("TERM", "xterm")])
          ~win32:false ~interactive:false))
    "zero CLICOLOR_FORCE does not enable redirected colors";
  check
    (Output.colors_enabled_with
       ~getenv:(environment [("TERM", "xterm")])
       ~win32:false ~interactive:true)
    "a Unix color terminal enables colors";
  check
    (not
       (Output.colors_enabled_with
          ~getenv:(environment [("TERM", "xterm"); ("CLICOLOR", "0")])
          ~win32:false ~interactive:true))
    "CLICOLOR zero disables terminal colors";
  check
    (not
       (Output.colors_enabled_with
          ~getenv:(environment [("TERM", "xterm"); ("NO_COLOR", "1")])
          ~win32:false ~interactive:true))
    "NO_COLOR disables Unix terminal colors";
  check
    (not
       (Output.colors_enabled_with ~getenv:(environment []) ~win32:false
          ~interactive:true))
    "a Unix terminal without TERM does not assume color support";
  check
    (Output.colors_enabled_with
       ~getenv:(environment [("TERM", "dumb"); ("NO_COLOR", "1")])
       ~win32:true ~interactive:true)
    "a Windows console does not use Unix terminal environment gates"
