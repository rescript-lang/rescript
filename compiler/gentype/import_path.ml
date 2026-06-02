open Gen_type_common

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

let emit (dir, s) = (dir, s) |> dump
