type t = Lsp.Uri.t

let strip_path = ref false (* for use in tests *)

let from_path path = Lsp.Uri.of_path path
let from_string str = Lsp.Uri.of_string str
let is_interface uri = uri |> Lsp.Uri.to_string |> Filename.check_suffix "i"
let to_path uri =
  (* Lsp.Uri.to_path remove the schema file:// but keep the `/` on start of path. *)
  let p = Lsp.Uri.to_path uri in
  if !strip_path then String.sub p 1 (String.length p - 1) else p

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
