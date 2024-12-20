(* Uncurried AST *)

let uncurried_type ~loc ~arity (t_arg : Parsetree.core_type) =
  let t_arg =
    match t_arg.ptyp_desc with
    | Ptyp_arrow (l, t1, t2, _) ->
      {t_arg with ptyp_desc = Ptyp_arrow (l, t1, t2, Some arity)}
    | _ -> assert false
  in
  Ast_helper.Typ.constr ~loc {txt = Lident "function$"; loc} [t_arg]

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

let make_uncurried_type ~env ~arity (t : Types.type_expr) =
  let lid : Longident.t = Lident "function$" in
  let path = Env.lookup_type lid env in
  let t =
    match t.desc with
    | Tarrow (l, t1, t2, c, _) ->
      {t with desc = Tarrow (l, t1, t2, c, Some arity)}
    | Tconstr _ -> assert false
    | Tvar _ -> t
    | _ -> assert false
  in
  Ctype.newconstr path [t]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident {name = "function$"}, [t], _) -> tarrow_to_arity t
  | _ -> assert false

let uncurried_type_get_arity_opt ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident {name = "function$"}, [t], _) -> Some (tarrow_to_arity t)
  | _ -> None

let remove_function_dollar ?env typ =
  match
    (match env with
    | Some env -> Ctype.expand_head env typ
    | None -> Ctype.repr typ)
      .desc
  with
  | Tconstr (Pident {name = "function$"}, [t], _) -> t
  | _ -> typ

let core_type_remove_function_dollar (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [t]) -> t
  | _ -> typ

let tcore_type_remove_function_dollar (typ : Typedtree.core_type) =
  match typ.ctyp_desc with
  | Ttyp_constr (Pident {name = "function$"}, _, [t]) -> t
  | _ -> typ
