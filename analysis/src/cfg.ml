let debug_follow_ctx_path = ref false

let is_doc_gen_from_compiler = ref false

let in_incremental_typechecking_mode =
  ref
    (try
       match Sys.getenv "RESCRIPT_INCREMENTAL_TYPECHECKING" with
       | "true" -> true
       | _ -> false
     with _ -> false)

let read_project_config_cache =
  ref
    (try
       match Sys.getenv "RESCRIPT_PROJECT_CONFIG_CACHE" with
       | "true" -> true
       | _ -> false
     with _ -> false)
