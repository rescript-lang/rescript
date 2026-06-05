let ( // ) = Filename.concat

let rescript_json = "rescript.json"
let bsconfig_json = "bsconfig.json"
let compiler_dir_partial_path = "lib" // "bs"
let compiler_ocaml_dir_partial_path = "lib" // "ocaml"
let compiler_log = ".compiler.log"
let sources_dirs = ".sourcedirs.json"
let compiler_log_partial_path = compiler_dir_partial_path // compiler_log
let compiler_info_partial_path =
  compiler_dir_partial_path // "compiler-info.json"
let build_ninja_partial_path = compiler_dir_partial_path // "build.ninja"
let rewatch_lock_partial_path = "lib" // "rewatch.lock"
let rescript_lock_partial_path = "lib" // "rescript.lock"
