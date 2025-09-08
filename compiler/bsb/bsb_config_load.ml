let ( // ) = Ext_path.combine

let load_json ~(per_proj_dir : string) ~(warn_legacy_config : bool) :
    string * Ext_json_types.t =
  let filename, abs, in_chan =
    let filename = Literals.rescript_json in
    let abs = per_proj_dir // filename in
    match open_in abs with
    | in_chan -> (filename, abs, in_chan)
    | exception e -> (
      let filename = Literals.bsconfig_json in
      let abs = per_proj_dir // filename in
      match open_in abs with
      | in_chan -> (filename, abs, in_chan)
      | exception _ -> raise e (* forward error from rescript.json *))
  in
  if warn_legacy_config && filename = Literals.bsconfig_json then
    print_endline
      "Warning: bsconfig.json is deprecated. Migrate it to rescript.json\n";
  match Ext_json_parse.parse_json_from_chan abs in_chan with
  | v ->
    close_in in_chan;
    (filename, v)
  | exception e ->
    close_in in_chan;
    raise e
