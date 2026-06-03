let white_list_side_effects =
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

let white_table_side_effects =
  lazy
    (let tbl = Hashtbl.create 11 in
     white_list_side_effects |> List.iter (fun s -> Hashtbl.add tbl s ());
     tbl)

let path_is_whitelisted_for_side_effects path =
  path
  |> Dce_path.on_ok_path ~when_contains_apply:false ~f:(fun s ->
         Hashtbl.mem (Lazy.force white_table_side_effects) s)

let rec expr_no_side_effects (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident _ | Texp_constant _ -> true
  | Texp_construct (_, _, el) -> el |> List.for_all expr_no_side_effects
  | Texp_function _ -> true
  (* Loop control changes whether subsequent code in the enclosing loop runs,
     so it should not be treated as a removable pure expression. *)
  | Texp_break | Texp_continue -> false
  | Texp_apply {funct = {exp_desc = Texp_ident (path, _, _)}; args}
    when path |> path_is_whitelisted_for_side_effects ->
    args |> List.for_all (fun (_, eo) -> eo |> expr_opt_no_side_effects)
  | Texp_apply _ -> false
  | Texp_sequence (e1, e2) ->
    e1 |> expr_no_side_effects && e2 |> expr_no_side_effects
  | Texp_let (_, vbs, e) ->
    vbs
    |> List.for_all (fun (vb : Typedtree.value_binding) ->
           vb.vb_expr |> expr_no_side_effects)
    && e |> expr_no_side_effects
  | Texp_record {fields; extended_expression} ->
    fields |> Array.for_all field_no_side_effects
    && extended_expression |> expr_opt_no_side_effects
  | Texp_assert _ -> false
  | Texp_match (e, cases_ok, cases_exn, partial) ->
    let cases = cases_ok @ cases_exn in
    partial = Total && e |> expr_no_side_effects
    && cases |> List.for_all case_no_side_effects
  | Texp_letmodule _ -> false
  | Texp_try (e, cases) ->
    e |> expr_no_side_effects && cases |> List.for_all case_no_side_effects
  | Texp_tuple el -> el |> List.for_all expr_no_side_effects
  | Texp_variant (_lbl, eo) -> eo |> expr_opt_no_side_effects
  | Texp_field (e, _lid, _ld) -> e |> expr_no_side_effects
  | Texp_setfield _ -> false
  | Texp_array el -> el |> List.for_all expr_no_side_effects
  | Texp_ifthenelse (e1, e2, eo) ->
    e1 |> expr_no_side_effects && e2 |> expr_no_side_effects
    && eo |> expr_opt_no_side_effects
  | Texp_while (e1, e2) ->
    e1 |> expr_no_side_effects && e2 |> expr_no_side_effects
  | Texp_for (_id, _pat, e1, e2, _dir, e3) ->
    e1 |> expr_no_side_effects && e2 |> expr_no_side_effects
    && e3 |> expr_no_side_effects
  | Texp_for_of _ | Texp_for_await_of _ -> false
  | Texp_send _ -> false
  | Texp_letexception (_ec, e) -> e |> expr_no_side_effects
  | Texp_pack _ -> false
  | Texp_extension_constructor _ when true -> true
  | _ -> (* on ocaml 4.08: Texp_letop | Texp_open *) true

and expr_opt_no_side_effects eo =
  match eo with
  | None -> true
  | Some e -> e |> expr_no_side_effects

and field_no_side_effects
    ((_ld, rld, _) : _ * Typedtree.record_label_definition * _) =
  match rld with
  | Kept _typeExpr -> true
  | Overridden (_lid, e) -> e |> expr_no_side_effects

and case_no_side_effects : Typedtree.case -> _ =
 fun {c_guard; c_rhs} ->
  c_guard |> expr_opt_no_side_effects && c_rhs |> expr_no_side_effects

let check_expr e = not (expr_no_side_effects e)
