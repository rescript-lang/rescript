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

(* parallel processing: number of domains to use (0 = sequential) *)
let parallel = ref 0

(* timing: report internal timing of analysis phases *)
let timing = ref false
