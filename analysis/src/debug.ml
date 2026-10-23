type debug_level = Off | Regular | Verbose

let debug_level = ref Off

let debug_print_env (env : Shared_types.Query_env.t) =
  env.path_rev @ [env.file.module_name] |> List.rev |> String.concat "."

let verbose () = !debug_level = Verbose
