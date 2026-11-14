let ( /+ ) = Filename.concat

let build_output_folder = "lib"
let compiler_dir_partial_path = build_output_folder /+ "rescript"
let compiler_log = ".compiler.log"
let sources_dirs = ".sourcedirs.json"
let compiler_log_partial_path = compiler_dir_partial_path /+ compiler_log
