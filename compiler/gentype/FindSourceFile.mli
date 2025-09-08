val cmt : Cmt_format.binary_annots -> string option
(** 
  [cmt annots] given [Cmt_format.binary_annots] it returns an absolute source file path
  if the file exists, otherwise it returns None.
  
  @param annots The binary annotations to be processed.
  @return An optional absolute path to the source file.
*)
