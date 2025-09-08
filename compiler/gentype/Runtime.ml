type module_item = string
type module_access_path =
  | Root of string
  | Dot of module_access_path * module_item

let new_module_item ~name = name

let rec emit_module_access_path ~config module_access_path =
  match module_access_path with
  | Root s -> s
  | Dot (p, module_item) ->
    p
    |> emit_module_access_path ~config
    |> EmitText.field_access ~label:module_item

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

let is_mutable_object_field name =
  String.length name >= 2
  && (String.sub name (String.length name - 2) 2 [@doesNotRaise]) = "#="

(** Mutable fields, i.e. fields annotated "[@set]"
   are represented as extra fields called "fieldName#="
   preceding the normal field. *)
let check_mutable_object_field ~previous_name ~name =
  previous_name = name ^ "#="

let default = "$$default"
