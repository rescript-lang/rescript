open GenTypeCommon

let shim_ts_output_file_extension ~(config : Config.t) =
  match config.module_resolution with
  | Node -> ".shim"
  | Node16 -> ".shim.js"
  | Bundler -> ".shim.ts"

let generated_files_extension ~(config : Config.t) =
  match config.generated_file_extension with
  | Some s ->
    (* from .foo.bar to .foo *)
    Filename.remove_extension s
  | None -> ".gen"

let ts_input_file_suffix ~(config : Config.t) =
  match config.generated_file_extension with
  | Some s when Filename.extension s <> "" (* double extension  *) -> s
  | _ -> generated_files_extension ~config ^ ".tsx"

let ts_output_file_suffix ~(config : Config.t) =
  generated_files_extension ~config ^ ".js"

let generated_module_extension ~(config : Config.t) =
  match config.module_resolution with
  | Node -> generated_files_extension ~config
  | Node16 -> ts_output_file_suffix ~config
  | Bundler -> ts_input_file_suffix ~config
