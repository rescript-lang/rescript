(* Uncurried AST *)

let uncurried_type ~arity (t_arg : Parsetree.core_type) =
  match t_arg.ptyp_desc with
  | Ptyp_arrow (l, t1, t2, _) ->
    {t_arg with ptyp_desc = Ptyp_arrow (l, t1, t2, Some arity)}
  | _ -> assert false

let uncurried_fun ~arity fun_expr =
  let fun_expr =
    match fun_expr.Parsetree.pexp_desc with
    | Pexp_fun f ->
      {fun_expr with pexp_desc = Pexp_fun {f with arity = Some arity}}
    | _ -> assert false
  in
  fun_expr

let expr_is_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun {arity = Some _} -> true
  | _ -> false

let expr_extract_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun {arity = Some _} -> expr
  | _ -> assert false
