(* the entry point. This is used by rescript_compiler_main.ml *)
let setup =
  lazy (match Sys.getenv_opt "BS_VSCODE" with 
      | Some ("true" | "1") -> 
        ()
      | Some _ | None -> 
        Super_location.setup ();
        Super_typetexp.setup ();
        Super_typemod.setup ();
        Super_typecore.setup ();
        Super_env.setup ())
