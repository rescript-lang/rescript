(** Dead code analysis - cmt file processing.
    Delegates to DceFileProcessing for AST traversal. *)

let processCmt = DceFileProcessing.process_cmt_file
