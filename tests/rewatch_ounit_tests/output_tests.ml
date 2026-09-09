open OUnit2

let check condition message = assert_bool message condition

let tests =
  "output_tests" >:: fun _context ->
  check
    (Output.cleanup_message ~step:"1/3" ~cleaned:2 ~total:5 ~seconds:1.5
    = "\027[2K\r[1/3] 🧹 Cleaned 2/5 in 1.50s")
    "interactive cleanup phase format";
  check
    (Output.compiler_cleanup_message ~step:"1/3"
    = "\027[2K\r[1/3] 🧹 Cleaned previous build due to compiler update")
    "interactive compiler cleanup format";
  check
    (Output.parsing_message ~step:"2/3" ~count:4 ~seconds:1.5
    = "\027[2K\r[2/3] 🧱 Parsed 4 source files in 1.50s")
    "interactive parsing phase format";
  check
    (Output.compiling_message ~step:"3/3" ~count:4 ~seconds:1.5
    = "\027[2K\r[3/3] 🤺 Compiled 4 modules in 1.50s")
    "interactive compilation phase format";
  check
    (Output.compilation_failed_message ~step:"2/2" ~count:3 ~seconds:1.5
    = "\027[2K\r[2/2] ❌ Compiled 3 modules in 1.50s")
    "interactive failed compilation phase format";
  check
    (Output.finished_compilation_message ~kind:None ~warnings:false ~seconds:1.5
    = "\027[2K\r✅ Finished compilation in 1.50s")
    "clean completion format";
  check
    (Output.finished_compilation_message ~kind:(Some "incremental")
       ~warnings:true ~seconds:1.5
    = "\027[2K\r⚠️ Finished incremental compilation with warnings in 1.50s")
    "warning completion format";
  check
    (Output.should_clear_screen ~clear_screen:true ~interactive:true)
    "interactive clear-screen";
  check
    (not (Output.should_clear_screen ~clear_screen:true ~interactive:false))
    "non-interactive clear-screen";
  check
    (not (Output.should_clear_screen ~clear_screen:false ~interactive:true))
    "disabled clear-screen"
