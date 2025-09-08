type debugLevel = Off | Regular | Verbose

let debugLevel = ref Off

let log s =
  match !debugLevel with
  | Regular | Verbose -> print_endline s
  | Off -> ()

let debugPrintEnv (env : SharedTypes.QueryEnv.t) =
  env.pathRev @ [env.file.moduleName] |> List.rev |> String.concat "."

let verbose () = !debugLevel = Verbose
