(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translating a path to the term that reads it. This is translation rather
   than representation, and is the only part that needed the type checker's
   environment. *)

open Lambda

(* Translate an access path *)

let rec transl_normal_path = function
  | Path.Pident id ->
    (* A predefined exception is its own name at runtime, so the reference is
       that string rather than a module. *)
    if Ident.is_predef_exn id then const (Const_string id.name)
    else if Ident.global id then global_module id
    else var id
  | Pdot (p, s, pos) ->
    prim
      ~primitive:(Pfield (pos, Fld_module {name = s}))
      ~args:[transl_normal_path p]
      Location.none
  | Papply _ -> assert false

(* Translation of identifiers *)

let transl_module_path ?(loc = Location.none) env path =
  transl_normal_path (Env.normalize_path (Some loc) env path)

let transl_value_path ?(loc = Location.none) env path =
  transl_normal_path (Env.normalize_path_prefix (Some loc) env path)

let transl_extension_path = transl_value_path

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)
