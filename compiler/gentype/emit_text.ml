type name_gen = (string, int) Hashtbl.t

let parens xs = "(" ^ (xs |> String.concat ", ") ^ ")"
let comment x = "/* " ^ x ^ " */"

let generics_string ~type_vars =
  match type_vars == [] with
  | true -> ""
  | false -> "<" ^ String.concat "," type_vars ^ ">"

(* Escape a semantic string as JavaScript/TypeScript string-literal contents.
   [String.escaped] cannot be used for this: its decimal byte escapes follow
   OCaml syntax and change non-ASCII UTF-8 text in JavaScript. *)
let escape_string_contents x =
  let buf = Buffer.create (String.length x) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
        Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    x;
  Buffer.contents buf

let quotes x = "\"" ^ x ^ "\""

let field_access ~label value = value ^ "." ^ label
