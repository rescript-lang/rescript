open Parsetree

let extension_name = function
  | Raw_expression -> "raw"
  | Raw_function -> "ffi"

let source_loc = function
  | Raw_string {loc} | Raw_template {loc} -> loc

let js_source = function
  | Raw_string {txt} -> txt
  | Raw_template {txt} -> String_literal.decode_raw_template_source txt

let payload_loc = function
  | Raw_string {loc} -> loc
  | Raw_template {loc} ->
    (* The template source ends before the closing backquote. *)
    if loc.loc_ghost then loc
    else
      {
        loc with
        loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + 1};
      }

let kind_of_name = function
  | "raw" -> Some Raw_expression
  | "ffi" -> Some Raw_function
  | _ -> None

let of_payload : payload -> raw_source option = function
  | PStr
      [
        {pstr_desc = Pstr_eval ({pexp_desc; pexp_loc; pexp_attributes = []}, [])};
      ] -> (
    match pexp_desc with
    | Pexp_constant (Pconst_string payload) ->
      Some
        (Raw_string {txt = String_literal.string_source payload; loc = pexp_loc})
    | Pexp_template {source_segments = [source]; values = []} ->
      Some (Raw_template source)
    | _ -> None)
  | _ -> None

(* Whether [source] can be written between double quotes as it is: it has no
   unescaped double quote. *)
let fits_double_quotes source =
  let rec loop index =
    if index >= String.length source then true
    else
      match String.unsafe_get source index with
      | '"' -> false
      | '\\' -> index + 1 < String.length source && loop (index + 2)
      | _ -> loop (index + 1)
  in
  loop 0

let to_extension ~name ~name_loc source : extension =
  let loc = payload_loc source in
  let expression =
    match source with
    | Raw_string source -> (
      match
        if fits_double_quotes source.txt then
          String_literal.string_from_source source.txt
        else None
      with
      | Some payload -> Ast_helper.Exp.constant ~loc (Pconst_string payload)
      | None -> Ast_helper.Exp.template ~loc [source] [])
    | Raw_template source -> Ast_helper.Exp.template ~loc [source] []
  in
  ({txt = name; loc = name_loc}, PStr [Ast_helper.Str.eval ~loc expression])

let uninterpreted ~loc name =
  Location.errorf ~loc "Uninterpreted extension '%s'." name
