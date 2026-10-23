type module_item = string
type module_access_path =
  | Root of string
  | Dot of module_access_path * module_item

let new_module_item ~name = name

(* The JS backend renames bindings and module fields that are JS keywords or
   globals, or that are not valid JS identifiers ([Ext_ident.convert]), except
   the top-level [default] export (see [Js_dump_import_export]). *)
let js_field_name name = Ext_ident.convert name

let js_export_name name = if name = "default" then name else js_field_name name

(* A name for a TS binding: renamed like the JS backend does unless it is
   already a valid binding name, so that a value named like a JS global such as
   [Array] keeps its name. *)
let ts_binding_name name =
  if
    Js_reserved_map.is_js_global name
    && not (Js_reserved_map.is_js_keyword name)
  then name
  else Ext_ident.convert name

let rec emit_module_access_path ~config module_access_path =
  match module_access_path with
  | Root s -> js_export_name s
  | Dot (p, module_item) ->
    p
    |> emit_module_access_path ~config
    |> Emit_text.field_access ~label:(js_field_name module_item)

let js_variant_tag ~polymorphic ~tag =
  match polymorphic with
  | true -> "NAME"
  | false -> (
    match tag with
    | Some tag -> tag
    | None -> "TAG")

let js_variant_payload_tag ~n = "_" ^ string_of_int n

let js_variant_value ~polymorphic =
  match polymorphic with
  | true -> "VAL"
  | false -> "value"

let default = "$$default"
