type debug_level = Off | Regular | Verbose

let debug_level = ref Off

let log s =
  match !debug_level with
  | Regular | Verbose -> print_endline s
  | Off -> ()

let debug_print_env (env : SharedTypes.QueryEnv.t) =
  env.path_rev @ [env.file.module_name] |> List.rev |> String.concat "."

let verbose () = !debug_level = Verbose
