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

(* What reanalyze needs from one value inclusion check. Keeping the two full
   value descriptions instead made up a fifth of the stdlib's .cmt bytes. *)
type value_dependency = {
  value_loc: Location.t;
  spec_loc: Location.t;
  optional_args: bool;
}

type cmt_infos = {
  cmt_modname: string;
  cmt_annots: binary_annots;
  cmt_value_dependencies: value_dependency list;
  cmt_source_digest: Digest.t option;
  cmt_extra_info: Cmt_utils.cmt_extra_info;
}

type error = Not_a_typedtree of string

open Tast_mapper

let cenv =
  {
    Tast_mapper.default with
    env = (fun _sub env -> Env.keep_only_local_constraints env);
  }

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
  match binary_annots with
  | Implementation s -> Implementation (cenv.structure cenv s)
  | Interface s -> Interface (cenv.signature cenv s)
  | Partial_implementation array ->
    Partial_implementation (Array.map clear_part array)
  | Partial_interface array -> Partial_interface (Array.map clear_part array)

exception Error of error

(** [read filename] reads a .cmi file as its cmi_infos, or a .cmt or .cmti
    file as its cmt_infos. *)
let read filename =
  (* Through the build file store, which holds the typed tree of the module
     being compiled until the build publishes it, without keeping the typed
     trees of other modules. *)
  let path = Compiler_request_state.resolve_path filename in
  let contents =
    match Build_file_store.read ~keep:false path with
    | Some contents -> contents
    | None -> raise (Sys_error (path ^ ": No such file or directory"))
  in
  let magic_length = String.length Config.cmt_magic_number in
  let magic_number =
    if String.length contents < magic_length then
      raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))
    else String.sub contents 0 magic_length
  in
  if magic_number = Config.cmt_magic_number then
    (None, Some (Marshal.from_string contents magic_length : cmt_infos))
  else if magic_number = Config.cmi_magic_number then
    (Some (Cmi_format.read_cmi_contents filename contents), None)
  else raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))

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

(* Whether a type is a function with an optional parameter. *)
let rec takes_optional_args (t : Types.type_expr) =
  match t.desc with
  | Tarrow (params, _) ->
    List.exists
      (fun ({lbl} : Types.arg) ->
        match lbl with
        | Optional _ -> true
        | Nolabel | Labelled _ -> false)
      params
  | Tlink t | Tsubst t -> takes_optional_args t
  | _ -> false

let record_value_dependency (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  if vd1.val_loc <> vd2.val_loc then
    let state = current_accumulators () in
    state.value_deps <- (vd1, vd2) :: state.value_deps

(* The flag is decided when the .cmt is written, not when the dependency is
   recorded: the inclusion check that follows the recording can still
   instantiate weak type variables of the value. *)
let value_dependencies () =
  List.map
    (fun ((vd1 : Types.value_description), (vd2 : Types.value_description)) ->
      {
        value_loc = vd1.val_loc;
        spec_loc = vd2.val_loc;
        optional_args =
          takes_optional_args vd1.val_type && takes_optional_args vd2.val_type;
      })
    (current_accumulators ()).value_deps
