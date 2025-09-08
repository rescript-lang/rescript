let rec interface items =
  match items with
  | {Typedtree.sig_loc} :: rest -> (
    match not (Sys.file_exists sig_loc.loc_start.pos_fname) with
    | true -> interface rest
    | false -> Some sig_loc.loc_start.pos_fname)
  | [] -> None

let rec implementation items =
  match items with
  | {Typedtree.str_loc} :: rest -> (
    match not (Sys.file_exists str_loc.loc_start.pos_fname) with
    | true -> implementation rest
    | false -> Some str_loc.loc_start.pos_fname)
  | [] -> None

let cmt cmt_annots =
  match cmt_annots with
  | Cmt_format.Interface signature ->
    if !Common.Cli.debug && signature.sig_items = [] then
      Log_.item "Interface %d@." (signature.sig_items |> List.length);
    interface signature.sig_items
  | Implementation structure ->
    if !Common.Cli.debug && structure.str_items = [] then
      Log_.item "Implementation %d@." (structure.str_items |> List.length);
    implementation structure.str_items
  | _ -> None
