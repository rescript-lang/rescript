let ( // ) = Ext_path.combine

let load_json ~(per_proj_dir : string) : string * Ext_json_types.t =
  let filename = Literals.rescript_json in
  let abs = per_proj_dir // filename in
  let in_chan = open_in abs in
  match Ext_json_parse.parse_json_from_chan abs in_chan with
  | v ->
    close_in in_chan;
    (filename, v)
  | exception e ->
    close_in in_chan;
    raise e
