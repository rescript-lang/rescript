let whiteListSideEffects =
  [
    "Pervasives./.";
    "Pervasives.ref";
    "Int64.mul";
    "Int64.neg";
    "Int64.sub";
    "Int64.shift_left";
    "Int64.one";
    "String.length";
  ]

let whiteTableSideEffects =
  lazy
    (let tbl = Hashtbl.create 11 in
     whiteListSideEffects |> List.iter (fun s -> Hashtbl.add tbl s ());
     tbl)

let pathIsWhitelistedForSideEffects path =
  path
  |> Common.Path.onOkPath ~whenContainsApply:false ~f:(fun s ->
         Hashtbl.mem (Lazy.force whiteTableSideEffects) s)

let rec exprNoSideEffects (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident _ | Texp_constant _ -> true
  | Texp_construct (_, _, el) -> el |> List.for_all exprNoSideEffects
  | Texp_function _ -> true
  | Texp_apply {funct = {exp_desc = Texp_ident (path, _, _)}; args}
    when path |> pathIsWhitelistedForSideEffects ->
    args |> List.for_all (fun (_, eo) -> eo |> exprOptNoSideEffects)
  | Texp_apply _ -> false
  | Texp_sequence (e1, e2) -> e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_let (_, vbs, e) ->
    vbs
    |> List.for_all (fun (vb : Typedtree.value_binding) ->
           vb.vb_expr |> exprNoSideEffects)
    && e |> exprNoSideEffects
  | Texp_record {fields; extended_expression} ->
    fields |> Array.for_all fieldNoSideEffects
    && extended_expression |> exprOptNoSideEffects
  | Texp_assert _ -> false
  | Texp_match (e, casesOk, casesExn, partial) ->
    let cases = casesOk @ casesExn in
    partial = Total && e |> exprNoSideEffects
    && cases |> List.for_all caseNoSideEffects
  | Texp_letmodule _ -> false
  | Texp_lazy e -> e |> exprNoSideEffects
  | Texp_try (e, cases) ->
    e |> exprNoSideEffects && cases |> List.for_all caseNoSideEffects
  | Texp_tuple el -> el |> List.for_all exprNoSideEffects
  | Texp_variant (_lbl, eo) -> eo |> exprOptNoSideEffects
  | Texp_field (e, _lid, _ld) -> e |> exprNoSideEffects
  | Texp_setfield _ -> false
  | Texp_array el -> el |> List.for_all exprNoSideEffects
  | Texp_ifthenelse (e1, e2, eo) ->
    e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
    && eo |> exprOptNoSideEffects
  | Texp_while (e1, e2) -> e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_for (_id, _pat, e1, e2, _dir, e3) ->
    e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
    && e3 |> exprNoSideEffects
  | Texp_send _ -> false
  | Texp_new _ -> true
  | Texp_instvar _ -> true
  | Texp_setinstvar _ -> false
  | Texp_override _ -> false
  | Texp_letexception (_ec, e) -> e |> exprNoSideEffects
  | Texp_pack _ -> false
  | Texp_extension_constructor _ when true -> true
  | _ -> (* on ocaml 4.08: Texp_letop | Texp_open *) true

and exprOptNoSideEffects eo =
  match eo with
  | None -> true
  | Some e -> e |> exprNoSideEffects

and fieldNoSideEffects
    ((_ld, rld, _) : _ * Typedtree.record_label_definition * _) =
  match rld with
  | Kept _typeExpr -> true
  | Overridden (_lid, e) -> e |> exprNoSideEffects

and caseNoSideEffects : Typedtree.case -> _ =
 fun {c_guard; c_rhs} ->
  c_guard |> exprOptNoSideEffects && c_rhs |> exprNoSideEffects

let checkExpr e = not (exprNoSideEffects e)
