open GenTypeCommon

let type_get_inlined ~config ~lookup_id ~type_name_is_interface type0 =
  let circular = ref "" in
  let rec visit ~(visited : StringSet.t) type_ =
    let normalized_ = type_ in
    match type_ with
    | Array (t, mutable_) ->
      let t_normalized = t |> visit ~visited in
      Array (t_normalized, mutable_)
    | Dict _ -> normalized_
    | Function ({arg_types; ret_type} as function_) ->
      let arg_converted =
        arg_types |> List.map (arg_type_to_grouped_arg ~visited)
      in
      let ret_normalized = ret_type |> visit ~visited in
      Function
        {function_ with arg_types = arg_converted; ret_type = ret_normalized}
    | Ident {builtin = true} -> normalized_
    | Ident {builtin = false; name; type_args} -> (
      if visited |> StringSet.mem name then (
        circular := name;
        normalized_)
      else
        let visited = visited |> StringSet.add name in
        match name |> lookup_id with
        | {CodeItem.annotation = GenTypeOpaque} -> normalized_
        | {annotation = NoGenType} -> normalized_
        | {type_vars; type_} ->
          let pairs =
            try List.combine type_vars type_args with Invalid_argument _ -> []
          in
          let f type_var =
            match
              pairs |> List.find (fun (type_var1, _) -> type_var = type_var1)
            with
            | _, type_argument -> Some type_argument
            | exception Not_found -> None
          in
          let inlined = type_ |> TypeVars.substitute ~f |> visit ~visited in
          inlined
        | exception Not_found ->
          let type_args =
            type_args |> List.map (fun t -> t |> visit ~visited)
          in
          Ident {builtin = false; name; type_args})
    | Null t ->
      let t_normalized = t |> visit ~visited in
      Null t_normalized
    | Nullable t ->
      let t_normalized = t |> visit ~visited in
      Nullable t_normalized
    | Object _ -> normalized_
    | Option t ->
      let t_normalized = t |> visit ~visited in
      Option t_normalized
    | Promise t ->
      let t_normalized = t |> visit ~visited in
      Promise t_normalized
    | Tuple inner_types ->
      let normalized_list = inner_types |> List.map (visit ~visited) in
      Tuple normalized_list
    | TypeVar _ -> normalized_
    | Variant variant ->
      let ordinary_variant = not variant.polymorphic in
      let with_payload_converted =
        variant.payloads
        |> List.map (fun (payload : payload) ->
               {payload with t = payload.t |> visit ~visited})
      in
      let normalized =
        match with_payload_converted with
        | [] when ordinary_variant -> normalized_
        | [payload] when ordinary_variant ->
          let normalized = Variant {variant with payloads = [payload]} in
          normalized
        | with_payload_converted ->
          Variant {variant with payloads = with_payload_converted}
      in
      normalized
  and arg_type_to_grouped_arg ~visited {a_name; a_type} =
    let t_normalized = a_type |> visit ~visited in
    {a_name; a_type = t_normalized}
  in
  let normalized = type0 |> visit ~visited:StringSet.empty in
  if !Debug.converter then
    Log_.item "type0:%s \n"
      (type0 |> EmitType.type_to_string ~config ~type_name_is_interface);
  normalized
