type load_error = string

val read_cmt : string -> (Cmt_format.cmt_infos, load_error) result

val implementation :
  string -> (Typedtree.structure * Cmt_format.cmt_infos, load_error) result

val interface :
  string -> (Typedtree.signature * Cmt_format.cmt_infos, load_error) result

