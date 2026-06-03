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

let set_item debug_item =
  match debug_item with
  | "all" -> set_all ()
  | "basic" -> basic := true
  | "codeItems" -> code_items := true
  | "config" -> config := true
  | "converter" -> converter := true
  | "dependencies" -> dependencies := true
  | "moduleResolution" -> module_resolution := true
  | "notImplemented" -> not_implemented := true
  | "translation" -> translation := true
  | "typeEnv" -> type_env := true
  | "typeResolution" -> type_resolution := true
  | _ -> ()
