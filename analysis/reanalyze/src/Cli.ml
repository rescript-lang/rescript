(** Command-line interface options for reanalyze.
    These refs are set by argument parsing in Reanalyze.ml *)

let debug = ref false
let ci = ref false

(** The command was a -cmt variant (e.g. -exception-cmt) *)
let cmtCommand = ref false

let experimental = ref false
let json = ref false

(* names to be considered live values *)
let liveNames = ref ([] : string list)

(* paths of files where all values are considered live *)
let livePaths = ref ([] : string list)

(* paths of files to exclude from analysis *)
let excludePaths = ref ([] : string list)

(* test flag: shuffle file order to verify order-independence *)
let testShuffle = ref false

(* timing: report internal timing of analysis phases *)
let timing = ref false

(* use reactive/incremental analysis (caches processed file_data) *)
let reactive = ref false

(* number of analysis runs (for benchmarking reactive mode) *)
let runs = ref 1

(* number of files to churn (remove/re-add) between runs for incremental testing *)
let churn = ref 0

(* output mermaid diagram of reactive pipeline *)
let mermaid = ref false
