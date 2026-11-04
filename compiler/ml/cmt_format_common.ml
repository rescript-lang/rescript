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

(* Shared CMT reading and collection logic. Persistence is supplied by
   Cmt_format_persistence, which compiler/ml/dune copies from platform/native
   or platform/playground according to the profile, so the playground does
   not retain the native writer and its transitive dependencies. *)

open Typedtree

type binary_annots =
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
  | Partial_structure of structure
  | Partial_structure_item of structure_item
  | Partial_expression of expression
  | Partial_pattern of pattern
  | Partial_signature of signature
  | Partial_signature_item of signature_item
  | Partial_module_type of module_type

type cmt_infos = {
  cmt_modname: string;
  cmt_annots: binary_annots;
  cmt_value_dependencies:
    (Types.value_description * Types.value_description) list;
  cmt_comments: (string * Location.t) list;
  cmt_args: string array;
  cmt_sourcefile: string option;
  cmt_builddir: string;
  cmt_loadpath: string list;
  cmt_source_digest: Digest.t option;
  cmt_initial_env: Env.t;
  cmt_imports: (string * Digest.t option) list;
  cmt_use_summaries: bool;
  cmt_extra_info: Cmt_utils.cmt_extra_info;
}

(* The boxed form of [cmt_infos] in [.cmt] files: the typed tree and the
   types of [Typedtree_boxed] and [Types_boxed]. *)
module Boxed = struct
  module T = Typedtree_boxed

  type binary_annots =
    | Implementation of T.structure
    | Interface of T.signature
    | Partial_implementation of binary_part array
    | Partial_interface of binary_part array

  and binary_part =
    | Partial_structure of T.structure
    | Partial_structure_item of T.structure_item
    | Partial_expression of T.expression
    | Partial_pattern of T.pattern
    | Partial_signature of T.signature
    | Partial_signature_item of T.signature_item
    | Partial_module_type of T.module_type

  type cmt_infos = {
    cmt_modname: string;
    cmt_annots: binary_annots;
    cmt_value_dependencies:
      (Types_boxed.value_description * Types_boxed.value_description) list;
    cmt_comments: (string * Location.t) list;
    cmt_args: string array;
    cmt_sourcefile: string option;
    cmt_builddir: string;
    cmt_loadpath: string list;
    cmt_source_digest: Digest.t option;
    cmt_initial_env: T.env;
    cmt_imports: (string * Digest.t option) list;
    cmt_use_summaries: bool;
    cmt_extra_info: Cmt_utils.cmt_extra_info;
  }
end

(* The whole [cmt_infos] is converted in one session, so that what it shares
   stays shared. *)
let export_cmt (cmt : cmt_infos) : Boxed.cmt_infos =
  Cmt_leaves.Export.run (fun c ->
      let module E = Typedtree_export in
      let part = function
        | Partial_structure s -> Boxed.Partial_structure (E.structure c s)
        | Partial_structure_item s ->
          Boxed.Partial_structure_item (E.structure_item c s)
        | Partial_expression e -> Boxed.Partial_expression (E.expression c e)
        | Partial_pattern p -> Boxed.Partial_pattern (E.pattern c p)
        | Partial_signature s -> Boxed.Partial_signature (E.signature c s)
        | Partial_signature_item s ->
          Boxed.Partial_signature_item (E.signature_item c s)
        | Partial_module_type m -> Boxed.Partial_module_type (E.module_type c m)
      in
      let cmt_annots : Boxed.binary_annots =
        match cmt.cmt_annots with
        | Implementation s -> Implementation (E.structure c s)
        | Interface s -> Interface (E.signature c s)
        | Partial_implementation parts ->
          Partial_implementation (Array.map part parts)
        | Partial_interface parts -> Partial_interface (Array.map part parts)
      in
      let vd = Cmt_leaves.Export.value_description c in
      let cmt_value_dependencies =
        List.map
          (fun (v1, v2) ->
            let v1 = vd v1 in
            (v1, vd v2))
          cmt.cmt_value_dependencies
      in
      {
        Boxed.cmt_modname = cmt.cmt_modname;
        cmt_annots;
        cmt_value_dependencies;
        cmt_comments = cmt.cmt_comments;
        cmt_args = cmt.cmt_args;
        cmt_sourcefile = cmt.cmt_sourcefile;
        cmt_builddir = cmt.cmt_builddir;
        cmt_loadpath = cmt.cmt_loadpath;
        cmt_source_digest = cmt.cmt_source_digest;
        cmt_initial_env = Cmt_leaves.Export.env c cmt.cmt_initial_env;
        cmt_imports = cmt.cmt_imports;
        cmt_use_summaries = cmt.cmt_use_summaries;
        cmt_extra_info = cmt.cmt_extra_info;
      })

let import_cmt (cmt : Boxed.cmt_infos) : cmt_infos =
  Cmt_leaves.Import.run (fun c ->
      let module I = Typedtree_import in
      let part : Boxed.binary_part -> binary_part = function
        | Partial_structure s -> Partial_structure (I.structure c s)
        | Partial_structure_item s ->
          Partial_structure_item (I.structure_item c s)
        | Partial_expression e -> Partial_expression (I.expression c e)
        | Partial_pattern p -> Partial_pattern (I.pattern c p)
        | Partial_signature s -> Partial_signature (I.signature c s)
        | Partial_signature_item s ->
          Partial_signature_item (I.signature_item c s)
        | Partial_module_type m -> Partial_module_type (I.module_type c m)
      in
      let cmt_annots =
        match cmt.cmt_annots with
        | Implementation s -> Implementation (I.structure c s)
        | Interface s -> Interface (I.signature c s)
        | Partial_implementation parts ->
          Partial_implementation (Array.map part parts)
        | Partial_interface parts -> Partial_interface (Array.map part parts)
      in
      let vd = Cmt_leaves.Import.value_description c in
      let cmt_value_dependencies =
        List.map
          (fun (v1, v2) ->
            let v1 = vd v1 in
            (v1, vd v2))
          cmt.cmt_value_dependencies
      in
      {
        cmt_modname = cmt.cmt_modname;
        cmt_annots;
        cmt_value_dependencies;
        cmt_comments = cmt.cmt_comments;
        cmt_args = cmt.cmt_args;
        cmt_sourcefile = cmt.cmt_sourcefile;
        cmt_builddir = cmt.cmt_builddir;
        cmt_loadpath = cmt.cmt_loadpath;
        cmt_source_digest = cmt.cmt_source_digest;
        cmt_initial_env = Cmt_leaves.Import.env c cmt.cmt_initial_env;
        cmt_imports = cmt.cmt_imports;
        cmt_use_summaries = cmt.cmt_use_summaries;
        cmt_extra_info = cmt.cmt_extra_info;
      })

(* Note that in Typerex, there is an awful hack to save a cmt file
   together with the interface file that was generated by ocaml (this
   is because the installed version of ocaml might differ from the one
   integrated in Typerex).
*)

let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  really_input_string ic len_magic_number

type error = Not_a_typedtree of string

let need_to_clear_env =
  try
    ignore (Sys.getenv "OCAML_BINANNOT_WITHENV");
    false
  with Not_found -> true

let keep_only_summary = Env.keep_only_summary

open Tast_mapper

let cenv =
  {Tast_mapper.default with env = (fun _sub env -> keep_only_summary env)}

let clear_part = function
  | Partial_structure s -> Partial_structure (cenv.structure cenv s)
  | Partial_structure_item s ->
    Partial_structure_item (cenv.structure_item cenv s)
  | Partial_expression e -> Partial_expression (cenv.expr cenv e)
  | Partial_pattern p -> Partial_pattern (cenv.pat cenv p)
  | Partial_signature s -> Partial_signature (cenv.signature cenv s)
  | Partial_signature_item s ->
    Partial_signature_item (cenv.signature_item cenv s)
  | Partial_module_type s -> Partial_module_type (cenv.module_type cenv s)

let clear_env binary_annots =
  if need_to_clear_env then
    match binary_annots with
    | Implementation s -> Implementation (cenv.structure cenv s)
    | Interface s -> Interface (cenv.signature cenv s)
    | Partial_implementation array ->
      Partial_implementation (Array.map clear_part array)
    | Partial_interface array -> Partial_interface (Array.map clear_part array)
  else binary_annots

exception Error of error

let input_cmt ic = import_cmt (input_value ic : Boxed.cmt_infos)

(** [read filename] reads a .cmi file as its cmi_infos, or a .cmt or .cmti
    file as its cmt_infos. *)
let read filename =
  (*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in_bin (Compiler_request_state.resolve_path filename) in
  try
    let magic_number = read_magic_number ic in
    let cmi, cmt =
      if magic_number = Config.cmt_magic_number then (None, Some (input_cmt ic))
      else if magic_number = Config.cmi_magic_number then
        (Some (Cmi_format.input_cmi ic), None)
      else raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))
    in
    close_in ic;
    (*    Printf.fprintf stderr "Cmt_format.read done\n%!"; *)
    (cmi, cmt)
  with e ->
    close_in ic;
    raise e

let read_cmt filename =
  match read filename with
  | _, None -> raise (Error (Not_a_typedtree filename))
  | _, Some cmt -> cmt

let read_cmi filename =
  match read filename with
  | None, _ -> raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))
  | Some cmi, _ -> cmi

(* These typed values cannot live in Compiler_request_state without a module
   dependency cycle. Keep their accumulator together in domain-local storage;
   the Cmt_utils callback below is shared initialization and only forwards to
   this domain's accumulator. *)
type accumulators = {
  mutable saved_types: binary_part list;
  mutable value_deps: (Types.value_description * Types.value_description) list;
  mutable deprecated_used: Cmt_utils.deprecated_used list;
}

let accumulators =
  Request_local.new_key (fun () ->
      {saved_types = []; value_deps = []; deprecated_used = []})

let current_accumulators () = Domain.DLS.get accumulators
let value_dependencies () = (current_accumulators ()).value_deps
let deprecated_uses () = (current_accumulators ()).deprecated_used

let clear () =
  let state = current_accumulators () in
  state.saved_types <- [];
  state.value_deps <- [];
  state.deprecated_used <- []

let add_saved_type b =
  let state = current_accumulators () in
  state.saved_types <- b :: state.saved_types

let get_saved_types () = (current_accumulators ()).saved_types
let set_saved_types l = (current_accumulators ()).saved_types <- l

let record_deprecated_used ?deprecated_context ?migration_template
    ?migration_in_pipe_chain_template source_loc deprecated_text =
  let state = current_accumulators () in
  state.deprecated_used <-
    {
      Cmt_utils.source_loc;
      deprecated_text;
      migration_template;
      migration_in_pipe_chain_template;
      context = deprecated_context;
    }
    :: state.deprecated_used

let _ = Cmt_utils.record_deprecated_used := record_deprecated_used

let record_value_dependency vd1 vd2 =
  if vd1.Types.val_loc <> vd2.Types.val_loc then
    let state = current_accumulators () in
    state.value_deps <- (vd1, vd2) :: state.value_deps
