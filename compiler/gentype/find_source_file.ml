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

let transform_to_absolute_path (path : string option) =
  let transform path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  Option.map transform path

let cmt cmt_annots =
  match cmt_annots with
  | Cmt_format.Interface signature ->
    transform_to_absolute_path (interface signature.sig_items)
  | Implementation structure ->
    transform_to_absolute_path (implementation structure.str_items)
  | _ -> None
