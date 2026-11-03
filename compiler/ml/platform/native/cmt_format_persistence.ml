(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmt_format_common

let set_args value = (Compiler_request_state.current ()).cmt_args <- value

let read_source_file path =
  let path = Compiler_request_state.resolve_path path in
  match Build_file_store.read path with
  | Some contents -> contents
  | None -> raise (Sys_error (path ^ ": No such file or directory"))

(* Unlike OCaml, no copy of the .cmi is written in front of the cmt infos:
   tools read the .cmi itself. *)
let save_cmt filename modname binary_annots sourcefile initial_env =
  if !((Clflags.current ()).binary_annotations) then
    let cmt =
      {
        cmt_modname = modname;
        cmt_annots = clear_env binary_annots;
        cmt_value_dependencies = value_dependencies ();
        cmt_comments = [];
        cmt_args = (Compiler_request_state.current ()).cmt_args;
        cmt_sourcefile = sourcefile;
        cmt_builddir = Compiler_request_state.cwd ();
        cmt_loadpath = Config.get_load_path ();
        cmt_source_digest =
          Misc.may_map
            (fun path -> Digest.string (read_source_file path))
            sourcefile;
        cmt_initial_env =
          (if need_to_clear_env then keep_only_summary initial_env
           else initial_env);
        cmt_imports = List.sort compare (Env.imports ());
        cmt_use_summaries = need_to_clear_env;
        cmt_extra_info = {deprecated_used = deprecated_uses ()};
      }
    in
    Build_file_store.write
      (Compiler_request_state.resolve_path filename)
      (Config.cmt_magic_number ^ Marshal.to_string (cmt : cmt_infos) [])
