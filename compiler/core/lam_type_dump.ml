let collect (lam : Lam.t) : unit =
  let let_count = ref 0 in
  let let_typed_count = ref 0 in
  let fn_count = ref 0 in
  let fn_typed_count = ref 0 in
  let apply_count = ref 0 in
  let apply_typed_count = ref 0 in
  let rec walk (lam : Lam.t) =
    match lam with
    | Lvar _ | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> ()
    | Lapply {ap_func; ap_args; ap_result_type; _} ->
      incr apply_count;
      (match ap_result_type with
      | Some ty ->
        incr apply_typed_count;
        Format.printf "  apply result: %a@." Printtyp.type_expr ty
      | None -> ());
      walk ap_func;
      List.iter walk ap_args
    | Lfunction {params = _; body; ty; _} ->
      incr fn_count;
      (match ty with
      | Some ty ->
        incr fn_typed_count;
        Format.printf "  function: %a@." Printtyp.type_expr ty
      | None -> ());
      walk body
    | Llet (_, id, ty, arg, body) ->
      incr let_count;
      (match ty with
      | Some ty ->
        incr let_typed_count;
        Format.printf "  let %s : %a@." id.name Printtyp.type_expr ty
      | None -> ());
      walk arg;
      walk body
    | Lletrec (bindings, body) ->
      List.iter (fun (_, l) -> walk l) bindings;
      walk body
    | Lprim {args; _} -> List.iter walk args
    | Lswitch (arg, sw) ->
      walk arg;
      List.iter (fun (_, l) -> walk l) sw.sw_consts;
      List.iter (fun (_, l) -> walk l) sw.sw_blocks;
      (match sw.sw_failaction with
      | Some l -> walk l
      | None -> ());
      List.iter (fun (_, l) -> walk l) sw.sw_consts;
      List.iter (fun (_, l) -> walk l) sw.sw_blocks
    | Lstringswitch (arg, cases, default) -> (
      walk arg;
      List.iter (fun (_, l) -> walk l) cases;
      match default with
      | Some l -> walk l
      | None -> ())
    | Lstaticraise (_, args) -> List.iter walk args
    | Lstaticcatch (e1, _, e2) ->
      walk e1;
      walk e2
    | Ltrywith (e1, _, e2) ->
      walk e1;
      walk e2
    | Lifthenelse (e1, e2, e3) ->
      walk e1;
      walk e2;
      walk e3
    | Lsequence (e1, e2) ->
      walk e1;
      walk e2
    | Lwhile (e1, e2) ->
      walk e1;
      walk e2
    | Lfor (_, e1, e2, _, e3) ->
      walk e1;
      walk e2;
      walk e3
    | Lfor_of (_, e1, e2) ->
      walk e1;
      walk e2
    | Lfor_await_of (_, e1, e2) ->
      walk e1;
      walk e2
    | Lassign (_, e) -> walk e
  in
  walk lam;
  Format.printf "Lam IR type summary:@.";
  Format.printf "  Llet: %d total, %d typed (%d%%)@." !let_count
    !let_typed_count
    (if !let_count > 0 then !let_typed_count * 100 / !let_count else 0);
  Format.printf "  Lfunction: %d total, %d typed (%d%%)@." !fn_count
    !fn_typed_count
    (if !fn_count > 0 then !fn_typed_count * 100 / !fn_count else 0);
  Format.printf "  Lapply: %d total, %d typed (%d%%)@." !apply_count
    !apply_typed_count
    (if !apply_count > 0 then !apply_typed_count * 100 / !apply_count else 0)
