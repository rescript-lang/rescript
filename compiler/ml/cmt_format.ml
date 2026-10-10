include Cmt_format_common

let save_cmt filename modname binary_annots sourcefile initial_env =
  Cmt_format_persistence.save_cmt filename modname binary_annots sourcefile
    initial_env;
  clear ()
