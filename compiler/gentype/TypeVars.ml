open GenTypeCommon

let extract_from_type_expr type_params =
  type_params
  |> List.fold_left
       (fun so_far type_expr ->
         match type_expr with
         | {Types.desc = Tvar (Some s)} ->
           let type_name = s in
           type_name :: so_far
         | {Types.desc = Tlink _} ->
           (* see if we need to collect more type vars here: t as 'a *)
           so_far
         | _ -> assert false)
       []
  |> List.rev

let extract_from_core_type type_params =
  type_params
  |> List.fold_left
       (fun so_far type_expr ->
         match type_expr.Typedtree.ctyp_desc with
         | Ttyp_var s ->
           let type_name = s in
           type_name :: so_far
         | _ -> so_far)
       []
  |> List.rev

let rec substitute ~f type0 =
  match type0 with
  | Array (t, array_kind) -> Array (t |> substitute ~f, array_kind)
  | Dict type_ -> Dict (type_ |> substitute ~f)
  | Function function_ ->
    Function
      {
        function_ with
        arg_types =
          function_.arg_types
          |> List.map (fun {a_name; a_type = t} ->
                 {a_name; a_type = t |> substitute ~f});
      }
  | Ident {type_args = []} -> type0
  | Ident ({type_args} as ident) ->
    Ident {ident with type_args = type_args |> List.map (substitute ~f)}
  | Null type_ -> Null (type_ |> substitute ~f)
  | Nullable type_ -> Nullable (type_ |> substitute ~f)
  | Object (closed_flag, fields) ->
    Object
      ( closed_flag,
        fields
        |> List.map (fun field ->
               {field with type_ = field.type_ |> substitute ~f}) )
  | Option type_ -> Option (type_ |> substitute ~f)
  | Promise type_ -> Promise (type_ |> substitute ~f)
  | Tuple inner_types -> Tuple (inner_types |> List.map (substitute ~f))
  | TypeVar s -> (
    match f s with
    | None -> type0
    | Some type1 -> type1)
  | Variant variant ->
    Variant
      {
        variant with
        payloads =
          variant.payloads
          |> List.map (fun payload ->
                 {payload with t = payload.t |> substitute ~f});
      }

let rec free_ type0 : StringSet.t =
  match type0 with
  | Array (t, _) -> t |> free_
  | Function {arg_types; ret_type; type_vars} ->
    StringSet.diff
      ((arg_types |> freeOfList_) +++ (ret_type |> free_))
      (type_vars |> StringSet.of_list)
  | Object (_, fields) ->
    fields
    |> List.fold_left
         (fun s {type_} -> StringSet.union s (type_ |> free_))
         StringSet.empty
  | Ident {type_args} ->
    type_args
    |> List.fold_left
         (fun s type_arg -> StringSet.union s (type_arg |> free_))
         StringSet.empty
  | Dict type_ | Null type_ | Nullable type_ -> type_ |> free_
  | Option type_ | Promise type_ -> type_ |> free_
  | Tuple inner_types ->
    inner_types
    |> List.fold_left
         (fun s type_arg -> StringSet.union s (type_arg |> free_))
         StringSet.empty
  | TypeVar s -> s |> StringSet.singleton
  | Variant {payloads} ->
    payloads
    |> List.fold_left
         (fun s {t} -> StringSet.union s (t |> free_))
         StringSet.empty

and freeOfList_ types =
  types
  |> List.fold_left (fun s {a_type} -> s +++ (a_type |> free_)) StringSet.empty

and ( +++ ) = StringSet.union

let free type_ = type_ |> free_ |> StringSet.elements
