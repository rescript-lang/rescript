module ParsetreeViewer = Res_parsetree_viewer
type kind = Parenthesized | Braced of Location.t | Nothing

let expr expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | _ -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let expr_record_row_rhs ~optional e =
  let kind = expr e in
  match kind with
  | Nothing when optional -> (
    match e.pexp_desc with
    | Pexp_ifthenelse _ | Pexp_fun _ -> Parenthesized
    | _ when ParsetreeViewer.is_binary_expression e -> Parenthesized
    | _ -> kind)
  | _ -> kind

let call_expr expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | _ -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filter_parsing_attrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | _
      when ParsetreeViewer.is_unary_expression expr
           || ParsetreeViewer.is_binary_expression expr ->
      Parenthesized
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _}
      when ParsetreeViewer.is_underscore_apply_sugar expr ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_constraint _ | Pexp_setfield _ | Pexp_match _ | Pexp_try _
       | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when Ast_uncurried.expr_is_uncurried_fun expr -> Parenthesized
    | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let structure_expr expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | _
      when ParsetreeViewer.has_attributes expr.pexp_attributes
           && not (ParsetreeViewer.is_jsx_expression expr) ->
      Parenthesized
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let unary_expr_operand expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filter_parsing_attrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | expr
      when ParsetreeViewer.is_unary_expression expr
           || ParsetreeViewer.is_binary_expression expr ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _}
      when ParsetreeViewer.is_underscore_apply_sugar expr ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_constraint _ | Pexp_setfield _
       | Pexp_extension _ (* readability? maybe remove *) | Pexp_match _
       | Pexp_try _ | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let binary_expr_operand ~is_lhs expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _}
      when ParsetreeViewer.is_underscore_apply_sugar expr ->
      Nothing
    | {pexp_desc = Pexp_constraint _ | Pexp_fun _ | Pexp_newtype _} ->
      Parenthesized
    | _ when Ast_uncurried.expr_is_uncurried_fun expr -> Parenthesized
    | expr when ParsetreeViewer.is_binary_expression expr -> Parenthesized
    | expr when ParsetreeViewer.is_ternary_expr expr -> Parenthesized
    | {pexp_desc = Pexp_lazy _ | Pexp_assert _} when is_lhs -> Parenthesized
    | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
      Parenthesized
    | {Parsetree.pexp_attributes = attrs} ->
      if ParsetreeViewer.has_printable_attributes attrs then Parenthesized
      else Nothing)

let sub_binary_expr_operand parent_operator child_operator =
  let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
  let prec_child = ParsetreeViewer.operator_precedence child_operator in
  prec_parent > prec_child
  || prec_parent == prec_child
     && not
          (ParsetreeViewer.flattenable_operators parent_operator child_operator)
  ||
  (* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… *)
  (parent_operator = "||" && child_operator = "&&")

let rhs_binary_expr_operand parent_operator rhs =
  match rhs.Parsetree.pexp_desc with
  | Parsetree.Pexp_apply
      {
        funct =
          {
            pexp_attributes = [];
            pexp_desc =
              Pexp_ident {txt = Longident.Lident operator; loc = operator_loc};
          };
        args = [(_, _left); (_, _right)];
      }
    when ParsetreeViewer.is_binary_operator operator
         && not (operator_loc.loc_ghost && operator = "++") ->
    let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
    let prec_child = ParsetreeViewer.operator_precedence operator in
    prec_parent == prec_child
  | _ -> false

let flatten_operand_rhs parent_operator rhs =
  match rhs.Parsetree.pexp_desc with
  | Parsetree.Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident {txt = Longident.Lident operator; loc = operator_loc};
          };
        args = [(_, _left); (_, _right)];
      }
    when ParsetreeViewer.is_binary_operator operator
         && not (operator_loc.loc_ghost && operator = "++") ->
    let prec_parent = ParsetreeViewer.operator_precedence parent_operator in
    let prec_child = ParsetreeViewer.operator_precedence operator in
    prec_parent >= prec_child || rhs.pexp_attributes <> []
  | Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _}) ->
    false
  | Pexp_fun _ | Pexp_newtype _ | Pexp_setfield _ | Pexp_constraint _ -> true
  | _ when ParsetreeViewer.is_ternary_expr rhs -> true
  | _ -> false

let binary_operator_inside_await_needs_parens operator =
  ParsetreeViewer.operator_precedence operator
  < ParsetreeViewer.operator_precedence "->"

let lazy_or_assert_or_await_expr_rhs ?(in_await = false) expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filter_parsing_attrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_apply
         {funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}}};
    }
      when ParsetreeViewer.is_binary_expression expr ->
      if in_await && not (binary_operator_inside_await_needs_parens operator)
      then Nothing
      else Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _}
      when ParsetreeViewer.is_underscore_apply_sugar expr ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_constraint _ | Pexp_setfield _ | Pexp_match _ | Pexp_try _
       | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _
      when (not in_await)
           && ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let is_negative_constant constant =
  let is_neg txt =
    let len = String.length txt in
    len > 0 && (String.get [@doesNotRaise]) txt 0 = '-'
  in
  match constant with
  | (Parsetree.Pconst_integer (i, _) | Pconst_float (i, _)) when is_neg i ->
    true
  | _ -> false

let field_expr expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filter_parsing_attrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | expr
      when ParsetreeViewer.is_binary_expression expr
           || ParsetreeViewer.is_unary_expression expr ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constant c} when is_negative_constant c -> Parenthesized
    | {pexp_desc = Pexp_fun _}
      when ParsetreeViewer.is_underscore_apply_sugar expr ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _
       | Pexp_extension _ (* %extension.x vs (%extension).x *) | Pexp_fun _
       | Pexp_newtype _ | Pexp_constraint _ | Pexp_setfield _ | Pexp_match _
       | Pexp_try _ | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let set_field_expr_rhs expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let ternary_operand expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ when Res_parsetree_viewer.is_fun_newtype expr -> (
      let _, _parameters, return_expr = ParsetreeViewer.fun_expr expr in
      match return_expr.pexp_desc with
      | Pexp_constraint _ -> Parenthesized
      | _ -> Nothing)
    | _ -> Nothing)

let starts_with_minus txt =
  let len = String.length txt in
  if len == 0 then false
  else
    let s = (String.get [@doesNotRaise]) txt 0 in
    s = '-'

let jsx_prop_expr expr =
  match expr.Parsetree.pexp_desc with
  | Parsetree.Pexp_let _ | Pexp_sequence _ | Pexp_letexception _
  | Pexp_letmodule _ | Pexp_open _ ->
    Nothing
  | _ -> (
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
    | None -> (
      match expr with
      | {
       Parsetree.pexp_desc =
         Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
       pexp_attributes = [];
      }
        when starts_with_minus x ->
        Parenthesized
      | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
        Parenthesized
      | {
       Parsetree.pexp_desc =
         ( Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _
         | Pexp_variant _ | Pexp_array _ | Pexp_pack _ | Pexp_record _
         | Pexp_extension _ | Pexp_letmodule _ | Pexp_letexception _
         | Pexp_open _ | Pexp_sequence _ | Pexp_let _ | Pexp_tuple _ );
       pexp_attributes = [];
      } ->
        Nothing
      | {
       Parsetree.pexp_desc =
         Pexp_constraint
           ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
       pexp_attributes = [];
      } ->
        Nothing
      | _ -> Parenthesized))

let jsx_child_expr expr =
  match expr.Parsetree.pexp_desc with
  | Parsetree.Pexp_let _ | Pexp_sequence _ | Pexp_letexception _
  | Pexp_letmodule _ | Pexp_open _ ->
    Nothing
  | _ -> (
    let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
    match opt_braces with
    | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
    | _ -> (
      match expr with
      | {
       Parsetree.pexp_desc =
         Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
       pexp_attributes = [];
      }
        when starts_with_minus x ->
        Parenthesized
      | _ when ParsetreeViewer.has_await_attribute expr.pexp_attributes ->
        Parenthesized
      | {
       Parsetree.pexp_desc =
         ( Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _
         | Pexp_variant _ | Pexp_array _ | Pexp_pack _ | Pexp_record _
         | Pexp_extension _ | Pexp_letmodule _ | Pexp_letexception _
         | Pexp_open _ | Pexp_sequence _ | Pexp_let _ );
       pexp_attributes = [];
      } ->
        Nothing
      | {
       Parsetree.pexp_desc =
         Pexp_constraint
           ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
       pexp_attributes = [];
      } ->
        Nothing
      | expr when ParsetreeViewer.is_jsx_expression expr -> Nothing
      | _ -> Parenthesized))

let binary_expr expr =
  let opt_braces, _ = ParsetreeViewer.process_braces_attr expr in
  match opt_braces with
  | Some ({Location.loc = braces_loc}, _) -> Braced braces_loc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = _ :: _} as expr
      when ParsetreeViewer.is_binary_expression expr ->
      Parenthesized
    | _ -> Nothing)

let mod_type_functor_return mod_type =
  match mod_type with
  | {Parsetree.pmty_desc = Pmty_with _} -> true
  | _ -> false

(* Add parens for readability:
     module type Functor = SetLike => Set with type t = A.t
   This is actually:
     module type Functor = (SetLike => Set) with type t = A.t
*)
let mod_type_with_operand mod_type =
  match mod_type with
  | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
  | _ -> false

let mod_expr_functor_constraint mod_type =
  match mod_type with
  | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
  | _ -> false

let braced_expr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _}) ->
    false
  | Pexp_constraint _ -> true
  | _ -> false

let include_mod_expr mod_expr =
  match mod_expr.Parsetree.pmod_desc with
  | Parsetree.Pmod_constraint _ -> true
  | _ -> false

let mod_expr_parens mod_expr =
  match mod_expr with
  | {
   Parsetree.pmod_desc =
     Pmod_constraint
       ( {Parsetree.pmod_desc = Pmod_structure _},
         {Parsetree.pmty_desc = Pmty_signature [{psig_desc = Psig_module _}]} );
  } ->
    false
  | {
   Parsetree.pmod_desc =
     Pmod_constraint
       (_, {Parsetree.pmty_desc = Pmty_signature [{psig_desc = Psig_module _}]});
  } ->
    true
  | _ -> false

let arrow_return_typ_expr typ_expr =
  match typ_expr.Parsetree.ptyp_desc with
  | Ptyp_arrow _ -> true
  | _ -> false

let pattern_record_row_rhs (pattern : Parsetree.pattern) =
  match pattern.ppat_desc with
  | Ppat_constraint ({ppat_desc = Ppat_unpack _}, {ptyp_desc = Ptyp_package _})
    ->
    false
  | Ppat_constraint _ -> true
  | _ -> false
