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
      (** The decoded Unicode code point of a character literal. For example,
          ['\u{1F600}'] is represented as [Const_char 0x1F600]. Source spelling
          has been discarded after type checking. *)
  | Const_string of string
      (** The decoded runtime value of an ordinary string literal. For example,
          ["a\\n"] is represented by a string containing an actual newline.
          Source spelling has been discarded after type checking. *)
  | Const_float of string
  | Const_bigint of bool * string

type template_segment = String_literal.template_segment
(** A segment of an ordinary backquoted template after validation. [source]
    preserves its spelling for JavaScript output; [semantic] is its decoded
    runtime string value. For example, the final segment of [`a ${value}\n`]
    preserves ["\\n"] as its source and contains an actual newline as its
    semantic value. Construct segments through [String_literal]. *)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

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
