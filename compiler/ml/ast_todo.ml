open Parsetree

let name = "todo"

let is_message = function
  | {
      pexp_desc =
        ( Pexp_constant (Pconst_string _)
        | Pexp_template {source_segments = [_]; values = []} );
      pexp_attributes = [];
    } ->
    true
  | _ -> false

let of_payload : payload -> expression option option = function
  | PStr [] -> Some None
  | PStr [{pstr_desc = Pstr_eval (message, [])}] when is_message message ->
    Some (Some message)
  | _ -> None

let to_extension ~name_loc message : extension =
  let payload =
    match message with
    | None -> []
    | Some message -> [Ast_helper.Str.eval ~loc:message.pexp_loc message]
  in
  ({txt = name; loc = name_loc}, PStr payload)
