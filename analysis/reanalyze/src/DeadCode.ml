(** Dead code analysis - cmt file processing.
    Delegates to DceFileProcessing for AST traversal. *)

let process_cmt = DceFileProcessing.process_cmt_file
