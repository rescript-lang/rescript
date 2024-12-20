(* Uncurried AST *)

let uncurried_type ~arity (t_arg : Parsetree.core_type) =
  match t_arg.ptyp_desc with
  | Ptyp_arrow (l, t1, t2, _) ->
    {t_arg with ptyp_desc = Ptyp_arrow (l, t1, t2, Some arity)}
  | _ -> assert false

let uncurried_fun ~arity fun_expr =
  let fun_expr =
    match fun_expr.Parsetree.pexp_desc with
    | Pexp_fun (l, eo, p, e, _) ->
      {fun_expr with pexp_desc = Pexp_fun (l, eo, p, e, Some arity)}
    | _ -> assert false
  in
  fun_expr

let expr_is_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, _, Some _) -> true
  | _ -> false

let expr_extract_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, _, Some _) -> expr
  | _ -> assert false

(* Typed AST *)

let tarrow_to_arity (t_arity : Types.type_expr) =
  match (Ctype.repr t_arity).desc with
  | Tarrow (_, _, _, _, Some arity) -> arity
  | Tarrow _ -> assert false
  | _ ->
    Format.eprintf "t: %a@." Printtyp.raw_type_expr t_arity;
    assert false

let tarrow_to_arity_opt (t_arity : Types.type_expr) =
  match (Ctype.repr t_arity).desc with
  | Tarrow (_, _, _, _, arity) -> arity
  | _ -> None

let uncurried_type_get_arity ~env typ =
  tarrow_to_arity (Ctype.expand_head env typ)

let uncurried_type_get_arity_opt ~env typ =
  tarrow_to_arity_opt (Ctype.expand_head env typ)
