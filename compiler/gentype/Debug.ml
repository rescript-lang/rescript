let basic = ref false
let code_items = ref false
let config = ref false
let converter = ref false
let dependencies = ref false
let module_resolution = ref false
let not_implemented = ref false
let translation = ref false
let type_env = ref false
let type_resolution = ref false

let set_all () =
  basic := true;
  code_items := true;
  config := true;
  converter := true;
  dependencies := true;
  module_resolution := true;
  not_implemented := true;
  translation := true;
  type_env := true;
  type_resolution := true

let set_item debug_item debug_value =
  let is_on =
    match debug_value with
    | Ext_json_types.True _ -> true
    | _ -> false
  in
  match debug_item with
  | "all" when is_on -> set_all ()
  | "basic" -> basic := is_on
  | "codeItems" -> code_items := is_on
  | "config" -> config := is_on
  | "converter" -> converter := is_on
  | "dependencies" -> dependencies := is_on
  | "moduleResolution" -> module_resolution := is_on
  | "notImplemented" -> not_implemented := is_on
  | "translation" -> translation := is_on
  | "typeEnv" -> type_env := is_on
  | "typeResolution" -> type_resolution := is_on
  | _ -> ()
