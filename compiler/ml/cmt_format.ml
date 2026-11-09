include Cmt_format_common

let save_cmt filename modname binary_annots sourcefile =
  Compiler_request_state.before_effect ();
  Cmt_format_persistence.save_cmt filename modname binary_annots sourcefile;
  clear ()
