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

(* Light weight, hopefully-enough-for-the-purpose fn to encode URI components.
   Built to handle the reserved characters listed in
   https://en.wikipedia.org/wiki/Percent-encoding. Note that this function is not
   general purpose, rather it's currently only for URL encoding the argument list
   passed to command links in markdown. *)
let encode_u_r_i_component text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '"' -> Buffer.add_string buf "%22"
      | '\'' -> Buffer.add_string buf "%22"
      | ':' -> Buffer.add_string buf "%3A"
      | ';' -> Buffer.add_string buf "%3B"
      | '/' -> Buffer.add_string buf "%2F"
      | '\\' -> Buffer.add_string buf "%5C"
      | ',' -> Buffer.add_string buf "%2C"
      | '&' -> Buffer.add_string buf "%26"
      | '[' -> Buffer.add_string buf "%5B"
      | ']' -> Buffer.add_string buf "%5D"
      | '#' -> Buffer.add_string buf "%23"
      | '$' -> Buffer.add_string buf "%24"
      | '+' -> Buffer.add_string buf "%2B"
      | '=' -> Buffer.add_string buf "%3D"
      | '?' -> Buffer.add_string buf "%3F"
      | '@' -> Buffer.add_string buf "%40"
      | '%' -> Buffer.add_string buf "%25"
      | c -> Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf
