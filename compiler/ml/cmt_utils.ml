type deprecated_used_context = FunctionCall | Reference

type deprecated_used = {
  source_loc: Location.t;
  deprecated_text: string;
  migration_template: Parsetree.expression option;
  migration_piped_template: Parsetree.expression option;
  context: deprecated_used_context option;
}

type cmt_extra_info = {deprecated_used: deprecated_used list}

let record_deprecated_used :
    (?deprecated_context:deprecated_used_context ->
    ?migration_template:Parsetree.expression ->
    ?migration_piped_template:Parsetree.expression ->
    Location.t ->
    string ->
    unit)
    ref =
  ref
    (fun
      ?deprecated_context ?migration_template ?migration_piped_template _ _ ->
      ignore deprecated_context;
      ignore migration_template;
      ignore migration_piped_template)
