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

let save_cmt ?capture filename modname binary_annots sourcefile initial_env cmi
    =
  if !((Clflags.current ()).binary_annotations) then
    Compiler_phase_trace.section "artifact.cmt_persist" (fun () ->
        let cmi_contents, interface_digest =
          match cmi with
          | None -> ("", None)
          | Some cmi ->
            let contents, digest = Cmi_format.encode_cmi cmi in
            (contents, Some digest)
        in
        let cmt =
          Compiler_phase_trace.section "artifact.cmt_prep" (fun () ->
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
                  Compiler_phase_trace.section "artifact.cmt_source_hash"
                    (fun () ->
                      Misc.may_map
                        (fun path ->
                          Digest.file (Compiler_request_state.resolve_path path))
                        sourcefile);
                cmt_initial_env =
                  (if need_to_clear_env then keep_only_summary initial_env
                   else initial_env);
                cmt_imports = List.sort compare (Env.imports ());
                cmt_interface_digest = interface_digest;
                cmt_use_summaries = need_to_clear_env;
                cmt_extra_info = {deprecated_used = deprecated_uses ()};
              })
        in
        let contents =
          Compiler_phase_trace.section "artifact.cmt_serialize" (fun () ->
              cmi_contents ^ Config.cmt_magic_number ^ Marshal.to_string cmt [])
        in
        let deferred =
          match capture with
          | None -> false
          | Some capture -> capture filename cmt contents
        in
        if not deferred then
          Misc.output_to_file_via_temporary ~mode:[Open_binary] filename
            (fun _ output_channel -> output_string output_channel contents);
        Some cmt)
  else if !((Clflags.current ()).bs_gentype) then
    match binary_annots with
    | Partial_implementation _ | Partial_interface _ -> None
    | Implementation _ | Interface _ | Packed _ ->
      Some
        {
          cmt_modname = modname;
          cmt_annots = clear_env binary_annots;
          cmt_value_dependencies = [];
          cmt_comments = [];
          cmt_args = [||];
          cmt_sourcefile = sourcefile;
          cmt_builddir = Compiler_request_state.cwd ();
          cmt_loadpath = Config.get_load_path ();
          cmt_source_digest =
            Misc.may_map
              (fun path ->
                Digest.file (Compiler_request_state.resolve_path path))
              sourcefile;
          cmt_initial_env = initial_env;
          cmt_imports = List.sort compare (Env.imports ());
          cmt_interface_digest = None;
          cmt_use_summaries = false;
          cmt_extra_info = {deprecated_used = []};
        }
  else None
