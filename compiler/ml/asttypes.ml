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

(** Auxiliary AST types used by parsetree and typedtree. *)

type constant =
  | Const_int of int
  | Const_char of int
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_bigint of bool * string

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type arity = int option

type 'a loc = 'a Location.loc = {txt: 'a; loc: Location.t}

type variance = Covariant | Contravariant | Invariant

type arg_label =
  | Nolabel (* x => ...*)
  | Labelled of string loc (*  ~label => ... *)
  | Optional of string loc (* ~(label=e) => ... *)

module Noloc = struct
  type arg_label =
    | Nolabel (* x => ...*)
    | Labelled of string (*  ~label => ... *)
    | Optional of string (* ~(label=e) => ... *)

  let same_arg_label (x : arg_label) y =
    match x with
    | Nolabel -> y = Nolabel
    | Labelled s -> (
      match y with
      | Labelled s0 -> s = s0
      | _ -> false)
    | Optional s -> (
      match y with
      | Optional s0 -> s = s0
      | _ -> false)
end

let to_arg_label ?(loc = Location.none) lbl =
  match lbl with
  | Noloc.Nolabel -> Nolabel
  | Labelled s -> Labelled {loc; txt = s}
  | Optional s -> Optional {loc; txt = s}

let to_noloc = function
  | Nolabel -> Noloc.Nolabel
  | Labelled {txt} -> Labelled txt
  | Optional {txt} -> Optional txt

let same_arg_label (x : arg_label) y =
  match x with
  | Nolabel -> y = Nolabel
  | Labelled {txt = s} -> (
    match y with
    | Labelled {txt = s0} -> s = s0
    | _ -> false)
  | Optional {txt = s} -> (
    match y with
    | Optional {txt = s0} -> s = s0
    | _ -> false)

let get_lbl_loc = function
  | Nolabel -> Location.none
  | Labelled {loc} | Optional {loc} -> loc
