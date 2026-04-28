val default_print_width : int

val print_type_params :
  (Parsetree.core_type * Asttypes.variance) list ->
  Res_comments_table.t ->
  Res_doc.t

val print_longident : Longident.t -> Res_doc.t

val print_typ_expr : Parsetree.core_type -> Res_comments_table.t -> Res_doc.t

val add_parens : Res_doc.t -> Res_doc.t

val print_expression : Parsetree.expression -> Res_comments_table.t -> Res_doc.t

val print_pattern : Parsetree.pattern -> Res_comments_table.t -> Res_doc.t
[@@live]

val print_structure : Parsetree.structure -> Res_comments_table.t -> Res_doc.t
[@@live]

val print_implementation :
  ?width:int -> Parsetree.structure -> comments:Res_comment.t list -> string
val print_interface :
  ?width:int -> Parsetree.signature -> comments:Res_comment.t list -> string

val print_ident_like :
  ?allow_uident:bool -> ?allow_hyphen:bool -> string -> Res_doc.t

val print_poly_var_ident : string -> Res_doc.t

val polyvar_ident_to_string : string -> string [@@live]
