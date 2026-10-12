(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printers for outcome trees, used by [Printtyp]. The ReScript printer lives in
   compiler/syntax, which depends on this library, so it is installed into
   these hooks by [Res_outcome_printer.setup]. *)

open Outcometree

let not_set_up _ _ = failwith "Oprint: Res_outcome_printer.setup was not run"

let out_type : (Format.formatter -> out_type -> unit) ref = ref not_set_up
let out_module_type : (Format.formatter -> out_module_type -> unit) ref =
  ref not_set_up
let out_sig_item : (Format.formatter -> out_sig_item -> unit) ref =
  ref not_set_up
let out_signature : (Format.formatter -> out_sig_item list -> unit) ref =
  ref not_set_up
let out_type_extension : (Format.formatter -> out_type_extension -> unit) ref =
  ref not_set_up
