let debugFollowCtxPath = ref false

let isDocGenFromCompiler = ref false

let inIncrementalTypecheckingMode =
  ref
    (try
       match Sys.getenv "RESCRIPT_INCREMENTAL_TYPECHECKING" with
       | "true" -> true
       | _ -> false
     with _ -> false)

let readProjectConfigCache =
  ref
    (try
       match Sys.getenv "RESCRIPT_PROJECT_CONFIG_CACHE" with
       | "true" -> true
       | _ -> false
     with _ -> false)
