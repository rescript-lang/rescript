type t = {path: string; uri: string}

let stripPath = ref false (* for use in tests *)

let pathToUri path =
  if Sys.os_type = "Unix" then "file://" ^ path
  else
    "file://"
    ^ (Str.global_replace (Str.regexp_string "\\") "/" path
      |> Str.substitute_first (Str.regexp "^\\([a-zA-Z]\\):") (fun text ->
             let name = Str.matched_group 1 text in
             "/" ^ String.lowercase_ascii name ^ "%3A"))

let fromPath path = {path; uri = pathToUri path}
let isInterface {path} = Filename.check_suffix path "i"
let toPath {path} = path

let toTopLevelLoc {path} =
  let topPos =
    {Lexing.pos_fname = path; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}
  in
  {Location.loc_start = topPos; Location.loc_end = topPos; loc_ghost = false}

let toString {uri} = if !stripPath then Filename.basename uri else uri

(* Light weight, hopefully-enough-for-the-purpose fn to encode URI components.
   Built to handle the reserved characters listed in
   https://en.wikipedia.org/wiki/Percent-encoding. Note that this function is not
   general purpose, rather it's currently only for URL encoding the argument list
   passed to command links in markdown. *)
let encodeURIComponent text =
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
