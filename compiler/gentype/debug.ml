(* GenType debug output belongs to the compiler request that enabled it. *)
let new_flag () = Domain.DLS.new_key (fun () -> ref false)

let basic_key = new_flag ()
let basic () = Domain.DLS.get basic_key
let code_items_key = new_flag ()
let code_items () = Domain.DLS.get code_items_key
let config_key = new_flag ()
let config () = Domain.DLS.get config_key
let converter_key = new_flag ()
let converter () = Domain.DLS.get converter_key
let dependencies_key = new_flag ()
let dependencies () = Domain.DLS.get dependencies_key
let module_resolution_key = new_flag ()
let module_resolution () = Domain.DLS.get module_resolution_key
let not_implemented_key = new_flag ()
let not_implemented () = Domain.DLS.get not_implemented_key
let translation_key = new_flag ()
let translation () = Domain.DLS.get translation_key
let type_env_key = new_flag ()
let type_env () = Domain.DLS.get type_env_key
let type_resolution_key = new_flag ()
let type_resolution () = Domain.DLS.get type_resolution_key

let reset () =
  basic () := false;
  code_items () := false;
  config () := false;
  converter () := false;
  dependencies () := false;
  module_resolution () := false;
  not_implemented () := false;
  translation () := false;
  type_env () := false;
  type_resolution () := false

let set_all () =
  basic () := true;
  code_items () := true;
  config () := true;
  converter () := true;
  dependencies () := true;
  module_resolution () := true;
  not_implemented () := true;
  translation () := true;
  type_env () := true;
  type_resolution () := true

let set_item debug_item =
  match debug_item with
  | "all" -> set_all ()
  | "basic" -> basic () := true
  | "codeItems" -> code_items () := true
  | "config" -> config () := true
  | "converter" -> converter () := true
  | "dependencies" -> dependencies () := true
  | "moduleResolution" -> module_resolution () := true
  | "notImplemented" -> not_implemented () := true
  | "translation" -> translation () := true
  | "typeEnv" -> type_env () := true
  | "typeResolution" -> type_resolution () := true
  | _ -> ()
