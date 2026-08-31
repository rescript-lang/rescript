open Gentype_common

type t = string * string

let bs_curry_path ~config = ("", Config.get_bs_curry_path ~config)

let from_module ~dir ~import_extension module_name =
  let with_no_path =
    (module_name |> Module_name.to_string
   |> Scoped_package.remove_generated_module)
    ^ import_extension
  in
  (dir, with_no_path)

let from_string_unsafe s = ("", s)

let chop_extension_safe (dir, s) =
  try (dir, s |> Filename.chop_extension) with Invalid_argument _ -> (dir, s)

let dump (dir, s) = Node_filename.concat dir s

let to_cmt ~(config : Config.t) ~output_file_relative (dir, s) =
  let open Filename in
  concat
    (output_file_relative |> dirname)
    ((dir, s) |> chop_extension_safe |> dump)
  ^ (match config.namespace with
    | None -> ""
    | Some name -> "-" ^ name)
  ^ ".cmt"

(* Import paths are emitted inside single-quoted JavaScript/TypeScript string
   literals. The AST stores their semantic value, so restore source escapes at
   this final output boundary. *)
let escape_for_single_quotes s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\'' -> Buffer.add_string buf "\\'"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
        Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let emit path = path |> dump |> escape_for_single_quotes
