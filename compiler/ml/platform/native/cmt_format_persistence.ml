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

let output_cmt output_channel cmt =
  output_string output_channel Config.cmt_magic_number;
  output_value output_channel (cmt : cmt_infos)

let save_cmt filename modname binary_annots sourcefile initial_env cmi =
  if !Clflags.binary_annotations then
    Misc.output_to_bin_file_directly filename
      (fun temp_file_name output_channel ->
        let interface_digest =
          match cmi with
          | None -> None
          | Some cmi ->
            Some (Cmi_format.output_cmi temp_file_name output_channel cmi)
        in
        let cmt =
          {
            cmt_modname = modname;
            cmt_annots = clear_env binary_annots;
            cmt_value_dependencies = value_dependencies ();
            cmt_comments = [];
            cmt_args = Sys.argv;
            cmt_sourcefile = sourcefile;
            cmt_builddir = Sys.getcwd ();
            cmt_loadpath = !Config.load_path;
            cmt_source_digest = Misc.may_map Digest.file sourcefile;
            cmt_initial_env =
              (if need_to_clear_env then keep_only_summary initial_env
               else initial_env);
            cmt_imports = List.sort compare (Env.imports ());
            cmt_interface_digest = interface_digest;
            cmt_use_summaries = need_to_clear_env;
            cmt_extra_info = {deprecated_used = deprecated_uses ()};
          }
        in
        output_cmt output_channel cmt)
