open GenTypeCommon

let name = "$GenTypeImport"

let alias = "type $GenTypeImport<Expected, T extends Expected> = T;"

let strict_name = "$GenTypeImportStrict"

let strict_alias =
  "type $GenTypeImportStrict<T, Expected extends T> = Expected;"

let rec should_inline_expected (t : type_) : bool =
  match t with
  | Ident {builtin = true; name; _} -> (
    match name with
    | "number" | "string" | "boolean" -> true
    | _ -> false)
  | Tuple inner -> inner |> List.for_all should_inline_expected
  | Array (t, _) -> should_inline_expected t
  | Promise t -> should_inline_expected t
  | _ -> false
