type t = Lsp.Uri.t

let strip_path = ref false (* for use in tests *)

let from_path path = Lsp.Uri.of_path path
let from_string str = Lsp.Uri.of_string str
let is_interface uri =
  Lsp.Uri.to_path uri |> String.ends_with ~suffix:Literals.suffix_resi
let to_path uri =
  (* Lsp.Uri.to_path remove the schema file:// but keep the `/` on start of path.
     Tests pass relative paths, which get a leading `/` this way, so tests strip
     it. Dependency files have absolute paths: keep those whole when only the
     whole path exists. *)
  let p = Lsp.Uri.to_path uri in
  if !strip_path then
    let stripped = String.sub p 1 (String.length p - 1) in
    if Sys.file_exists p && not (Sys.file_exists stripped) then p else stripped
  else p

let to_top_level_loc (uri : Lsp.Uri.t) =
  let top_pos =
    {
      Lexing.pos_fname = uri |> Lsp.Uri.to_path;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
  in
  {Location.loc_start = top_pos; Location.loc_end = top_pos; loc_ghost = false}

let to_string t =
  if !strip_path then Filename.basename (Lsp.Uri.to_path t)
  else Lsp.Uri.to_string t
