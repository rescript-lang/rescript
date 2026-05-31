(**
 * `startsWith(string, prefix)`
 * true if the string starts with the prefix
 *)
let starts_with s prefix =
  if prefix = "" then true
  else
    let p = String.length prefix in
    p <= String.length s && String.sub s 0 p = prefix

let ends_with s suffix =
  if suffix = "" then true
  else
    let p = String.length suffix in
    let l = String.length s in
    p <= String.length s && String.sub s (l - p) p = suffix

let is_first_char_uppercase s =
  String.length s > 0 && Char.equal s.[0] (Char.uppercase_ascii s.[0])

let cmt_pos_to_position {Lexing.pos_lnum; pos_cnum; pos_bol} =
  Lsp.Types.Position.create ~line:(pos_lnum - 1) ~character:(pos_cnum - pos_bol)

let cmt_loc_to_range {Location.loc_start; loc_end} =
  let start = cmt_pos_to_position loc_start in
  let end_ = cmt_pos_to_position loc_end in
  Lsp.Types.Range.create ~start ~end_

let end_of_location loc length =
  let open Location in
  {
    loc with
    loc_start = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

let chop_location_end loc length =
  let open Location in
  {
    loc with
    loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

(** An optional List.find *)
let rec find fn items =
  match items with
  | [] -> None
  | one :: rest -> (
    match fn one with
    | None -> find fn rest
    | Some x -> Some x)

let filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with
      | None -> aux accu l
      | Some v -> aux (v :: accu) l)
  in
  aux []

let dump_path path = Str.global_replace (Str.regexp_string "\\") "/" path
let is_uncurried_internal path = starts_with (Path.name path) "Js.Fn.arity"

let flatten_long_ident ?(jsx = false) ?(cut_at_offset = None) lid =
  let extend_path s path =
    match path with
    | "" :: _ -> path
    | _ -> s :: path
  in
  let rec loop lid =
    match lid with
    | Longident.Lident txt -> ([txt], String.length txt)
    | Ldot (lid, txt) ->
      let path, offset = loop lid in
      if Some offset = cut_at_offset then (extend_path "" path, offset + 1)
      else if jsx && txt = "createElement" then (path, offset)
      else if txt = "_" then (extend_path "" path, offset + 1)
      else (extend_path txt path, offset + 1 + String.length txt)
    | Lapply _ -> ([], 0)
  in
  let path, _ = loop lid in
  List.rev path

let identify_pexp pexp =
  match pexp with
  | Parsetree.Pexp_ident _ -> "Pexp_ident"
  | Pexp_constant _ -> "Pexp_constant"
  | Pexp_let _ -> "Pexp_let"
  | Pexp_fun _ -> "Pexp_fun"
  | Pexp_apply _ -> "Pexp_apply"
  | Pexp_match _ -> "Pexp_match"
  | Pexp_try _ -> "Pexp_try"
  | Pexp_tuple _ -> "Pexp_tuple"
  | Pexp_construct _ -> "Pexp_construct"
  | Pexp_variant _ -> "Pexp_variant"
  | Pexp_record _ -> "Pexp_record"
  | Pexp_field _ -> "Pexp_field"
  | Pexp_setfield _ -> "Pexp_setfield"
  | Pexp_array _ -> "Pexp_array"
  | Pexp_ifthenelse _ -> "Pexp_ifthenelse"
  | Pexp_sequence _ -> "Pexp_sequence"
  | Pexp_break -> "Pexp_break"
  | Pexp_continue -> "Pexp_continue"
  | Pexp_while _ -> "Pexp_while"
  | Pexp_for _ -> "Pexp_for"
  | Pexp_for_of _ -> "Pexp_for_of"
  | Pexp_for_await_of _ -> "Pexp_for_await_of"
  | Pexp_constraint _ -> "Pexp_constraint"
  | Pexp_coerce _ -> "Pexp_coerce"
  | Pexp_send _ -> "Pexp_send"
  | Pexp_letmodule _ -> "Pexp_letmodule"
  | Pexp_letexception _ -> "Pexp_letexception"
  | Pexp_assert _ -> "Pexp_assert"
  | Pexp_newtype _ -> "Pexp_newtype"
  | Pexp_pack _ -> "Pexp_pack"
  | Pexp_extension _ -> "Pexp_extension"
  | Pexp_open _ -> "Pexp_open"
  | Pexp_await _ -> "Pexp_await"
  | Pexp_jsx_element _ -> "Pexp_jsx_element"

let identify_ppat pat =
  match pat with
  | Parsetree.Ppat_any -> "Ppat_any"
  | Ppat_var _ -> "Ppat_var"
  | Ppat_alias _ -> "Ppat_alias"
  | Ppat_constant _ -> "Ppat_constant"
  | Ppat_interval _ -> "Ppat_interval"
  | Ppat_tuple _ -> "Ppat_tuple"
  | Ppat_construct _ -> "Ppat_construct"
  | Ppat_variant _ -> "Ppat_variant"
  | Ppat_record _ -> "Ppat_record"
  | Ppat_array _ -> "Ppat_array"
  | Ppat_or _ -> "Ppat_or"
  | Ppat_constraint _ -> "Ppat_constraint"
  | Ppat_type _ -> "Ppat_type"
  | Ppat_unpack _ -> "Ppat_unpack"
  | Ppat_exception _ -> "Ppat_exception"
  | Ppat_extension _ -> "Ppat_extension"
  | Ppat_open _ -> "Ppat_open"

let rec skip_white text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skip_white text (i - 1)
    | _ -> i

let has_braces attributes =
  attributes |> List.exists (fun (loc, _) -> loc.Location.txt = "res.braces")

let rec unwrap_if_option (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> unwrap_if_option t1
  | Tconstr (Path.Pident {name = "option"}, [unwrapped_type], _) ->
    unwrapped_type
  | _ -> t

let is_jsx_component (vb : Parsetree.value_binding) =
  vb.pvb_attributes
  |> List.exists (function
       | {Location.txt = "react.component" | "jsx.component"}, _payload -> true
       | _ -> false)

let check_name name ~prefix ~exact =
  if exact then name = prefix else starts_with name prefix

let rec get_unqualified_name txt =
  match txt with
  | Longident.Lident field_name -> field_name
  | Ldot (t, _) -> get_unqualified_name t
  | _ -> ""

let indent n text =
  let spaces = String.make n ' ' in
  let len = String.length text in
  let text =
    if len != 0 && text.[len - 1] = '\n' then String.sub text 0 (len - 1)
    else text
  in
  let lines = String.split_on_char '\n' text in
  match lines with
  | [] -> ""
  | [line] -> line
  | line :: lines ->
    line ^ "\n"
    ^ (lines |> List.map (fun line -> spaces ^ line) |> String.concat "\n")

let mk_position (pos : Pos.t) =
  let line, character = pos in
  Lsp.Types.Position.create ~line ~character

let range_of_loc (loc : Location.t) =
  let start = loc |> Loc.start |> mk_position in
  let end_ = loc |> Loc.end_ |> mk_position in
  Lsp.Types.Range.create ~start ~end_

let rec expand_path (path : Path.t) =
  match path with
  | Pident id -> [Ident.name id]
  | Pdot (p, s, _) -> s :: expand_path p
  | Papply _ -> []

module Option = struct
  let flat_map f o =
    match o with
    | None -> None
    | Some v -> f v
end

let rec last_elements list =
  match list with
  | ([_; _] | [_] | []) as res -> res
  | _ :: tl -> last_elements tl

let lowercase_first_char s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) s

let cut_after_dash s =
  match String.index s '-' with
  | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
  | exception Not_found -> s

let file_name_has_unallowed_chars s =
  let regexp = Str.regexp "[^A-Za-z0-9_]" in
  try
    ignore (Str.search_forward regexp s 0);
    true
  with Not_found -> false

(* Flattens any namespace in the provided path.
   Example:
    Globals-RescriptBun.URL.t (which is an illegal path because of the namespace) becomes:
    RescriptBun.Globals.URL.t
*)
let rec flatten_any_namespace_in_path path =
  match path with
  | [] -> []
  | head :: tail ->
    if String.contains head '-' then
      let parts = String.split_on_char '-' head in
      (* Namespaces are in reverse order, so "URL-RescriptBun" where RescriptBun is the namespace. *)
      (parts |> List.rev) @ flatten_any_namespace_in_path tail
    else head :: flatten_any_namespace_in_path tail

let print_maybe_exotic_ident ?(allow_uident = false) txt =
  let len = String.length txt in

  let rec loop i =
    if i == len then txt
    else if i == 0 then
      match String.unsafe_get txt i with
      | 'A' .. 'Z' when allow_uident -> loop (i + 1)
      | 'a' .. 'z' | '_' -> loop (i + 1)
      | _ -> "\"" ^ txt ^ "\""
    else
      match String.unsafe_get txt i with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> loop (i + 1)
      | _ -> "\"" ^ txt ^ "\""
  in
  if Res_token.is_keyword_txt txt then "\"" ^ txt ^ "\"" else loop 0

let find_package_json root =
  let path = Uri.to_path root in

  let rec loop path =
    if path = "/" then None
    else if Files.exists (Filename.concat path "package.json") then
      Some (Filename.concat path "package.json")
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop path
