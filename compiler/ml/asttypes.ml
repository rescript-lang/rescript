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

type arg_label =
  | Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)

type arity = int option

type 'a loc = 'a Location.loc = {txt: 'a; loc: Location.t}

type variance = Covariant | Contravariant | Invariant

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

type arg_label_loc =
  | Nolbl
  | Lbl of string loc (*  label:T -> ... *)
  | Opt of string loc (* ?label:T -> ... *)

let to_arg_label_loc ?(loc = Location.none) lbl =
  match lbl with
  | Nolabel -> Nolbl
  | Labelled s -> Lbl {loc; txt = s}
  | Optional s -> Opt {loc; txt = s}

let to_arg_label = function
  | Nolbl -> Nolabel
  | Lbl {txt} -> Labelled txt
  | Opt {txt} -> Optional txt

let same_arg_label_loc (x : arg_label_loc) y =
  match x with
  | Nolbl -> y = Nolbl
  | Lbl {txt = s} -> (
    match y with
    | Lbl {txt = s0} -> s = s0
    | _ -> false)
  | Opt {txt = s} -> (
    match y with
    | Opt {txt = s0} -> s = s0
    | _ -> false)

let get_lbl_loc = function
  | Nolbl -> Location.none
  | Lbl {loc} | Opt {loc} -> loc
