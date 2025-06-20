let version = "4.06.1+BS"

(* FIXME: Unreliable resolution *)
let standard_library =
  let ( // ) = Filename.concat in
  let exe_path = Sys.executable_name in
  if Ext_string.contain_substring exe_path ("node_modules" // "@rescript") then
    (* node_modules/@rescript/{platform}/bin *)
    Filename.dirname exe_path // Filename.parent_dir_name
    // Filename.parent_dir_name // Filename.parent_dir_name // "rescript"
    // "lib" // "ocaml"
  else if Ext_string.contain_substring exe_path ("node_modules" // "rescript")
  then
    (* node_modules/rescript/{platform} *)
    Filename.dirname exe_path // Filename.parent_dir_name // "lib" // "ocaml"
  else
    (* git repo: rescript/packages/@rescript/{platform}/bin *)
    Filename.dirname exe_path // Filename.parent_dir_name
    // Filename.parent_dir_name // Filename.parent_dir_name
    // Filename.parent_dir_name // "lib" // "ocaml"

let standard_library_default = standard_library

let unsafe_empty_array = ref false

let cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"

and ast_intf_magic_number = "Caml1999N022"

and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)

(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;

  (* print the magic number *)
  p "cmi_magic_number" cmi_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  flush oc
