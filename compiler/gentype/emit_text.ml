type name_gen = (string, int) Hashtbl.t

let parens xs = "(" ^ (xs |> String.concat ", ") ^ ")"
let comment x = "/* " ^ x ^ " */"

let generics_string ~type_vars =
  match type_vars == [] with
  | true -> ""
  | false -> "<" ^ String.concat "," type_vars ^ ">"

let quotes x = "\"" ^ x ^ "\""

let field_access ~label value = value ^ "." ^ label
