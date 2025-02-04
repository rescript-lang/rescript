type debugLevel = Off | Regular | Verbose

let debugLevel = ref Off

let debugPrintEnv (env : SharedTypes.QueryEnv.t) =
  env.pathRev @ [env.file.moduleName] |> List.rev |> String.concat "."

(**
Log formatted message to the std_out when the debugLevel is Regular or Verbose.
Automatically appends a trailing newline.
*)
let regular fmt =
  match !debugLevel with
  | Regular | Verbose -> Format.printf (fmt ^^ "\n")
  | Off -> Format.ifprintf Format.std_formatter fmt

(**
Log formatted message to the std_out when the debugLevel is Verbose.
Automatically appends a trailing newline.
*)
let verbose fmt =
  match !debugLevel with
  | Off | Regular -> Format.ifprintf Format.std_formatter fmt
  | Verbose -> Format.printf (fmt ^^ "\n")
