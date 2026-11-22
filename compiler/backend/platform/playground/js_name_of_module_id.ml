(* The playground bundle keeps runtime modules under ./stdlib. *)
let string_of_module_id (module_id : Lam_module_ident.t)
    ~output_dir:(_ : string) (_module_system : Js_packages_info.module_system) =
  match module_id.kind with
  | External {name} -> name
  | Runtime | Ml -> "./stdlib/" ^ module_id.id.name ^ ".mjs"
