module StringMap = Map.Make (String)

let t_equals t1 t2 = t1.Types.level = t2.Types.level && t1.id = t2.id

let substitute_types ~type_map (t : Types.type_expr) =
  if StringMap.is_empty type_map then t
  else
    let apply_substitution type_variable_name t =
      match StringMap.find_opt type_variable_name type_map with
      | None -> t
      | Some substituted_type -> substituted_type
    in
    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> {t with desc = Tlink (loop t)}
      | Tvar (Some type_variable_name) ->
        apply_substitution type_variable_name t
      | Tvar None -> t
      | Tunivar _ -> t
      | Tconstr (path, args, _memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, ref Types.Mnil)}
      | Tsubst t -> {t with desc = Tsubst (loop t)}
      | Tvariant rd -> {t with desc = Tvariant (row_desc rd)}
      | Tnil -> t
      | Tarrow (arg, ret, c, arity) ->
        {
          t with
          desc = Tarrow ({arg with typ = loop arg.typ}, loop ret, c, arity);
        }
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and row_desc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, row_field rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and row_field (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let substitute_type_vars (type_vars : (string * Types.type_expr) list)
    (typ : Types.type_expr) =
  let type_map =
    type_vars
    |> List.fold_left
         (fun acc (tvar_name, tvar_typ) -> StringMap.add tvar_name tvar_typ acc)
         StringMap.empty
  in
  substitute_types ~type_map typ

let has_type_spread (lbls : Typedtree.label_declaration list) =
  lbls
  |> List.exists (fun (l : Typedtree.label_declaration) ->
         match l with
         | {ld_name = {txt = "..."}} -> true
         | _ -> false)

let extract_type_vars (type_params : Types.type_expr list)
    (typ : Types.type_expr) =
  (* The type variables applied to the record spread itself. *)
  let applied_type_vars =
    match Ctype.repr typ with
    | {desc = Tpoly ({desc = Tconstr (_, tvars, _)}, _)} -> tvars
    | _ -> []
  in
  if List.length type_params = List.length applied_type_vars then
    (* Track which type param in the record we're spreading
       belongs to which type variable applied to the spread itself. *)
    let paired_type_vars = List.combine type_params applied_type_vars in
    paired_type_vars
    |> List.filter_map (fun (t, applied_tvar) ->
           match t.Types.desc with
           | Tvar (Some tname) -> Some (tname, applied_tvar)
           | _ -> None)
  else []

let expand_labels_with_type_spreads (env : Env.t)
    (lbls : Typedtree.label_declaration list)
    (lbls' : Types.label_declaration list) =
  match has_type_spread lbls with
  | false -> Some (lbls, lbls')
  | true ->
    let rec extract (t : Types.type_expr) =
      match t.desc with
      | Tpoly (t, []) -> extract t
      | _ -> Ctype.repr t
    in
    let mk_lbl (l : Types.label_declaration) (ld_type : Typedtree.core_type)
        (type_vars : (string * Types.type_expr) list) :
        Typedtree.label_declaration =
      {
        ld_id = l.ld_id;
        ld_name = {txt = Ident.name l.ld_id; loc = l.ld_loc};
        ld_mutable = l.ld_mutable;
        ld_optional = l.ld_optional;
        ld_type =
          {ld_type with ctyp_type = substitute_type_vars type_vars l.ld_type};
        ld_loc = l.ld_loc;
        ld_attributes = l.ld_attributes;
      }
    in
    let rec process_lbls acc (lbls : Typedtree.label_declaration list)
        (lbls' : Types.label_declaration list) =
      match (lbls, lbls') with
      | {ld_name = {txt = "..."}; ld_type} :: rest, _ :: rest' -> (
        match
          Ctype.extract_concrete_typedecl env (extract ld_type.ctyp_type)
        with
        | _p0, _p, {type_kind = Type_record (fields, _repr); type_params} ->
          let type_vars = extract_type_vars type_params ld_type.ctyp_type in
          process_lbls
            ( fst acc @ Ext_list.map fields (fun l -> mk_lbl l ld_type type_vars),
              snd acc
              @ Ext_list.map fields (fun l ->
                    {l with ld_type = substitute_type_vars type_vars l.ld_type})
            )
            rest rest'
        | _ -> None
        | exception _ -> None)
      | lbl :: rest, lbl' :: rest' ->
        process_lbls (fst acc @ [lbl], snd acc @ [lbl']) rest rest'
      | _ -> Some acc
    in
    process_lbls ([], []) lbls lbls'
