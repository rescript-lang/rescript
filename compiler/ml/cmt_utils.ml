type deprecated_used = {
  source_loc: Location.t;
  deprecated_text: string;
  migration_template: Parsetree.expression option;
}

type cmt_extra_info = {deprecated_used: deprecated_used list}

let record_deprecated_used :
    (Location.t -> string -> Parsetree.expression option -> unit) ref =
  ref (fun _ _ _ -> ())
