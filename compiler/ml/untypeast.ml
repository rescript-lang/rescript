(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let constant = function
  | Const_char c -> Pconst_char c
  | Const_string (s, d) -> Pconst_string (s, d)
  | Const_int i -> Pconst_integer (string_of_int i, None)
  | Const_int32 i -> Pconst_integer (Int32.to_string i, Some 'l')
  | Const_int64 i -> Pconst_integer (Int64.to_string i, Some 'L')
  | Const_bigint (sign, i) ->
    Pconst_integer (Bigint_utils.to_string sign i, Some 'n')
  | Const_float f -> Pconst_float (f, None)
