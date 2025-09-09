let helper_type_name = "$RescriptTypeSatisfiesTypeScriptType"

let is_helper_ident name = name = helper_type_name

let render_helper_ident ~rendered_name ~rendered_type_args
    ~rendered_type_args_default =
  match rendered_type_args with
  | [a1; a2] ->
    let render_arg s = "  " ^ s in
    rendered_name ^ "<\n" ^ render_arg a1 ^ ",\n" ^ render_arg a2 ^ "\n>"
  | _ ->
    rendered_name
    ^ EmitText.generics_string ~type_vars:rendered_type_args_default

let emit_helper_alias ~emitters =
  let alias =
    "export type $RescriptTypeSatisfiesTypeScriptType<RescriptType, \
     TypeScriptType extends RescriptType> = TypeScriptType;"
  in
  Emitters.export_early ~emitters alias
