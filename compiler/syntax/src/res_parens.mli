type kind = Parenthesized | Braced of Location.t | Nothing

val expr : Parsetree.expression -> kind
val structure_expr : Parsetree.expression -> kind

val unary_expr_operand : Parsetree.expression -> kind

val binary_expr_operand : is_lhs:bool -> Parsetree.expression -> kind
val sub_binary_expr_operand : string -> string -> bool
val rhs_binary_expr_operand : string -> Parsetree.expression -> bool
val flatten_operand_rhs : string -> Parsetree.expression -> bool

val binary_operator_inside_await_needs_parens : string -> bool
val assert_or_await_expr_rhs : ?in_await:bool -> Parsetree.expression -> kind

val field_expr : Parsetree.expression -> kind

val set_field_expr_rhs : Parsetree.expression -> kind

val ternary_operand : Parsetree.expression -> kind

val jsx_prop_expr : Parsetree.expression -> kind
val jsx_child_expr : Parsetree.expression -> kind

val binary_expr : Parsetree.expression -> kind
val mod_type_functor_return : Parsetree.module_type -> bool
val mod_type_with_operand : Parsetree.module_type -> bool
val mod_expr_functor_constraint : Parsetree.module_type -> bool

val braced_expr : Parsetree.expression -> bool
val call_expr : Parsetree.expression -> kind

val include_mod_expr : Parsetree.module_expr -> bool

val mod_expr_parens : Parsetree.module_expr -> bool

val arrow_return_typ_expr : Parsetree.core_type -> bool

val pattern_record_row_rhs : Parsetree.pattern -> bool

val expr_record_row_rhs : optional:bool -> Parsetree.expression -> kind
