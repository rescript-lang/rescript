(* Restructures a nested tree of arrow types into its args & returnType
 * The parsetree contains: a => b => c => d, for printing purposes
 * we restructure the tree into (a, b, c) and its returnType d *)
val arrow_type :
  ?max_arity:int ->
  Parsetree.core_type ->
  Parsetree.attributes
  * (Parsetree.attributes * Asttypes.arg_label * Parsetree.core_type) list
  * Parsetree.core_type

val functor_type :
  Parsetree.module_type ->
  (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option)
  list
  * Parsetree.module_type

val has_await_attribute : Parsetree.attributes -> bool
val has_inline_record_definition_attribute : Parsetree.attributes -> bool
val has_res_pat_variant_spread_attribute : Parsetree.attributes -> bool
val has_dict_pattern_attribute : Parsetree.attributes -> bool

type if_condition_kind =
  | If of Parsetree.expression
  | IfLet of Parsetree.pattern * Parsetree.expression

(* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
 * The purpose of this function is to flatten nested ifs into one sequence.
 * Basically compute: ([if, else if, else if, else if], else) *)
val collect_if_expressions :
  Parsetree.expression ->
  (Location.t * if_condition_kind * Parsetree.expression) list
  * Parsetree.expression option

val collect_array_expressions :
  Parsetree.expression ->
  Parsetree.expression list * Parsetree.expression option

val collect_list_expressions :
  Parsetree.expression ->
  Parsetree.expression list * Parsetree.expression option

type fun_param_kind =
  | Parameter of {
      attrs: Parsetree.attributes;
      lbl: Asttypes.arg_label;
      default_expr: Parsetree.expression option;
      pat: Parsetree.pattern;
    }
  | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

val fun_expr :
  Parsetree.expression -> bool * fun_param_kind list * Parsetree.expression

(* example:
 *  `makeCoordinate({
 *    x: 1,
 *    y: 2,
 *  })`
 *  Notice howe `({` and `})` "hug" or stick to each other *)
val is_huggable_expression : Parsetree.expression -> bool

val is_huggable_pattern : Parsetree.pattern -> bool

val is_huggable_rhs : Parsetree.expression -> bool

val operator_precedence : string -> int

val is_unary_expression : Parsetree.expression -> bool
val is_binary_operator : string -> bool
val is_binary_expression : Parsetree.expression -> bool
val is_rhs_binary_operator : string -> bool

val flattenable_operators : string -> string -> bool

val has_attributes : Parsetree.attributes -> bool

val is_array_access : Parsetree.expression -> bool
val is_ternary_expr : Parsetree.expression -> bool
val is_if_let_expr : Parsetree.expression -> bool

val collect_ternary_parts :
  Parsetree.expression ->
  (Parsetree.expression * Parsetree.expression) list * Parsetree.expression

val parameters_should_hug : fun_param_kind list -> bool

val filter_ternary_attributes : Parsetree.attributes -> Parsetree.attributes
val filter_fragile_match_attributes :
  Parsetree.attributes -> Parsetree.attributes

val is_jsx_expression : Parsetree.expression -> bool
val has_jsx_attribute : Parsetree.attributes -> bool

val should_indent_binary_expr : Parsetree.expression -> bool
val should_inline_rhs_binary_expr : Parsetree.expression -> bool
val has_printable_attributes : Parsetree.attributes -> bool
val filter_printable_attributes : Parsetree.attributes -> Parsetree.attributes
val partition_printable_attributes :
  Parsetree.attributes -> Parsetree.attributes * Parsetree.attributes

val requires_special_callback_printing_last_arg :
  (Asttypes.arg_label * Parsetree.expression) list -> bool
val requires_special_callback_printing_first_arg :
  (Asttypes.arg_label * Parsetree.expression) list -> bool

val mod_expr_apply :
  Parsetree.module_expr -> Parsetree.module_expr list * Parsetree.module_expr

(* Collection of utilities to view the ast in a more a convenient form,
 * allowing for easier processing.
 * Example: given a ptyp_arrow type, what are its arguments and what is the
 * returnType? *)

val mod_expr_functor :
  Parsetree.module_expr ->
  (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option)
  list
  * Parsetree.module_expr

val collect_patterns_from_list_construct :
  Parsetree.pattern list ->
  Parsetree.pattern ->
  Parsetree.pattern list * Parsetree.pattern

val is_block_expr : Parsetree.expression -> bool

val is_template_literal : Parsetree.expression -> bool
val is_tagged_template_literal : Parsetree.expression -> bool
val has_template_literal_attr : Parsetree.attributes -> bool

val is_spread_belt_list_concat : Parsetree.expression -> bool

val is_spread_belt_array_concat : Parsetree.expression -> bool

val collect_or_pattern_chain : Parsetree.pattern -> Parsetree.pattern list

val process_braces_attr :
  Parsetree.expression -> Parsetree.attribute option * Parsetree.expression

val filter_parsing_attrs : Parsetree.attributes -> Parsetree.attributes

val is_braced_expr : Parsetree.expression -> bool

val is_single_pipe_expr : Parsetree.expression -> bool

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
val rewrite_underscore_apply : Parsetree.expression -> Parsetree.expression

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
val is_underscore_apply_sugar : Parsetree.expression -> bool

val has_if_let_attribute : Parsetree.attributes -> bool

val is_rewritten_underscore_apply_sugar : Parsetree.expression -> bool

val is_fun_newtype : Parsetree.expression -> bool

val is_tuple_array : Parsetree.expression -> bool
