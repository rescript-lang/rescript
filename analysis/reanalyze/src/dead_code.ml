(** Dead code analysis - cmt file processing.
    Delegates to DceFileProcessing for AST traversal. *)

let process_cmt = Dce_file_processing.process_cmt_file
