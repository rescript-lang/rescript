let remove_function_dollar (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident {name = "function$"}, [t], _) -> t
  | _ -> typ
