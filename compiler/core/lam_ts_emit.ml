let rec path_to_ts (path : Path.t) : string =
  match path with
  | Pident id -> Ident.name id
  | Pdot (p, s, _) ->
    let parent = path_to_ts p in
    parent ^ "." ^ s
  | Papply _ -> "unknown"

let var_to_ts (name : string) : string =
  let buf = Buffer.create (String.length name + 1) in
  Buffer.add_char buf (Char.uppercase_ascii name.[0]);
  Buffer.add_string buf (String.sub name 1 (String.length name - 1));
  Buffer.contents buf

let collect_type_vars (ty : Types.type_expr) : Types.type_expr list =
  let seen = Hashtbl.create 8 in
  let acc = ref [] in
  let rec visit (ty : Types.type_expr) =
    match ty.Types.desc with
    | Tvar (Some _) ->
      if not (Hashtbl.mem seen ty) then (
        Hashtbl.add seen ty ();
        acc := ty :: !acc)
    | Tarrow (arg, ret_ty, _, _) ->
      visit arg.typ;
      visit ret_ty
    | Ttuple ls -> List.iter visit ls
    | Tconstr (_, args, _) -> List.iter visit args
    | Tlink t -> visit t
    | Tpoly (t, _) -> visit t
    | _ -> ()
  in
  visit ty;
  List.rev !acc

let type_var_to_ts (ty : Types.type_expr) : string =
  match ty.desc with
  | Tvar (Some s) -> var_to_ts s
  | _ -> "unknown"

let rec type_expr_to_ts (ty : Types.type_expr) : string =
  match ty.desc with
  | Tvar None -> "unknown"
  | Tvar (Some s) -> var_to_ts s
  | Tarrow ({lbl = Nolabel; typ = arg_ty}, ret_ty, _, _) ->
    let arg = type_expr_to_ts arg_ty in
    let ret = type_expr_to_ts ret_ty in
    "(_: " ^ arg ^ ") => " ^ ret
  | Tarrow ({lbl = Labelled {txt = l}; typ = arg_ty}, ret_ty, _, _) ->
    let arg = type_expr_to_ts arg_ty in
    let ret = type_expr_to_ts ret_ty in
    "(" ^ l ^ ": " ^ arg ^ ") => " ^ ret
  | Tarrow ({lbl = Optional {txt = l}; typ = arg_ty}, ret_ty, _, _) ->
    let arg = type_expr_to_ts arg_ty in
    let ret = type_expr_to_ts ret_ty in
    "(" ^ l ^ "?: " ^ arg ^ ") => " ^ ret
  | Ttuple tys -> "[" ^ String.concat ", " (List.map type_expr_to_ts tys) ^ "]"
  | Tconstr (path, args, _) ->
    let name = path_to_ts path in
    let name =
      match name with
      | "int" -> "number"
      | "float" -> "number"
      | "string" -> "string"
      | "bool" -> "boolean"
      | "unit" -> "void"
      | "array" -> "Array"
      | "list" -> "Array"
      | "option" -> "unknown"
      | "promise" -> "Promise"
      | _ -> name
    in
    if args = [] then name
    else name ^ "<" ^ String.concat ", " (List.map type_expr_to_ts args) ^ ">"
  | Tlink ty -> type_expr_to_ts ty
  | Tpoly (ty, []) -> type_expr_to_ts ty
  | Tpoly (ty, tvs) ->
    let params = List.map type_var_to_ts tvs in
    "<" ^ String.concat ", " params ^ ">" ^ type_expr_to_ts ty
  | Tnil -> "undefined"
  | _ -> "unknown"

let type_expr_to_ts_with_generics (ty : Types.type_expr) : string =
  let body = type_expr_to_ts ty in
  let vars = collect_type_vars ty in
  match vars with
  | [] -> body
  | _ ->
    let params = List.map type_var_to_ts vars in
    "<" ^ String.concat ", " params ^ ">" ^ body

let rec function_arity (lam : Lam.t) : int option =
  match lam with
  | Lam.Lfunction {arity; _} -> Some arity
  | Lam.Llet (_, _, _, _, body) -> function_arity body
  | _ -> None

let core_type_to_ts (ct : Typedtree.core_type) : string =
  match ct.Typedtree.ctyp_desc with
  | Typedtree.Ttyp_var s -> var_to_ts s
  | _ -> type_expr_to_ts ct.Typedtree.ctyp_type

let params_to_ts (params : (Typedtree.core_type * _) list) : string =
  match params with
  | [] -> ""
  | _ ->
    let names = List.map (fun (ct, _) -> core_type_to_ts ct) params in
    "<" ^ String.concat ", " names ^ ">"

let emit_type_decl ppf (td : Typedtree.type_declaration) =
  let name = td.Typedtree.typ_name.Location.txt in
  let params = params_to_ts td.Typedtree.typ_params in
  let body =
    match td.Typedtree.typ_kind with
    | Typedtree.Ttype_abstract -> (
      match td.Typedtree.typ_manifest with
      | Some manifest -> core_type_to_ts manifest
      | None -> "unknown")
    | Typedtree.Ttype_variant ctors ->
      let parts =
        List.filter_map
          (fun (cd : Typedtree.constructor_declaration) ->
            let cname = cd.Typedtree.cd_name.Location.txt in
            match cd.Typedtree.cd_args with
            | Typedtree.Cstr_tuple [] -> Some ("\"" ^ cname ^ "\"")
            | Typedtree.Cstr_tuple args ->
              let arg_types = List.map core_type_to_ts args in
              let fields =
                List.mapi
                  (fun i ty -> "_" ^ string_of_int i ^ ": " ^ ty)
                  arg_types
              in
              Some
                ("{ TAG: \"" ^ cname ^ "\"; " ^ String.concat "; " fields ^ " }")
            | Typedtree.Cstr_record _ -> None)
          ctors
      in
      if parts = [] then "unknown" else String.concat " | " parts
    | Typedtree.Ttype_record labels ->
      let fields =
        List.map
          (fun (ld : Typedtree.label_declaration) ->
            let fname = ld.Typedtree.ld_name.Location.txt in
            let ftype = core_type_to_ts ld.Typedtree.ld_type in
            (if ld.Typedtree.ld_optional then fname ^ "?" else fname)
            ^ ": " ^ ftype)
          labels
      in
      "{ " ^ String.concat "; " fields ^ " }"
    | Typedtree.Ttype_open -> "unknown"
  in
  Format.fprintf ppf "export type %s%s = %s@." name params body

let emit_type_decls ppf (str : Typedtree.structure) =
  List.iter
    (fun item ->
      match item.Typedtree.str_desc with
      | Typedtree.Tstr_type (_, decls) -> List.iter (emit_type_decl ppf) decls
      | _ -> ())
    str.Typedtree.str_items

let emit_decls ?(typedtree : Typedtree.structure option) ppf
    (groups : Lam_group.t list) (exports : Ident.t list) =
  Format.fprintf ppf "// Type declarations generated by ReScript@.@.";
  (match typedtree with
  | Some str -> emit_type_decls ppf str
  | None -> ());
  let export_set = Set_ident.of_list exports in
  let walk_group (group : Lam_group.t) =
    match group with
    | Lam_group.Single (_, id, ty, lam) when Set_ident.mem export_set id -> (
      let ts_ty =
        match ty with
        | Some ty -> Some (type_expr_to_ts_with_generics ty)
        | None -> (
          match lam with
          | Lam.Lfunction {ty = Some ty; _} ->
            Some (type_expr_to_ts_with_generics ty)
          | _ -> None)
      in
      let is_fn = function_arity lam in
      match (is_fn, ts_ty) with
      | Some _, Some ts -> Format.fprintf ppf "export const %s: %s@." id.name ts
      | Some _, None ->
        Format.fprintf ppf "export function %s(...args: unknown[]): unknown@."
          id.name
      | None, Some ts -> Format.fprintf ppf "export const %s: %s@." id.name ts
      | None, None -> Format.fprintf ppf "export const %s: unknown@." id.name)
    | Lam_group.Recursive bindings ->
      List.iter
        (fun (id, lam) ->
          if Set_ident.mem export_set id then
            match function_arity lam with
            | Some _ ->
              Format.fprintf ppf
                "export function %s(...args: unknown[]): unknown@." id.name
            | None -> Format.fprintf ppf "export const %s: unknown@." id.name)
        bindings
    | _ -> ()
  in
  List.iter walk_group groups
