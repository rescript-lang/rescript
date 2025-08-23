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

(* Detection of partial matches and unused match cases. *)
open Asttypes
open Typedtree
open Types

val top_pretty : Format.formatter -> pattern -> unit
val pretty_pat : pattern -> unit
val pretty_line : pattern list -> unit
val pretty_matrix : pattern list list -> unit

val print_res_pat : (Typedtree.pattern -> string) ref

val omega : pattern
val omegas : int -> pattern list
val omega_list : 'a list -> pattern list
val normalize_pat : pattern -> pattern
val all_record_args :
  (Longident.t loc * label_description * pattern * bool) list ->
  (Longident.t loc * label_description * pattern * bool) list
val const_compare : constant -> constant -> int

val le_pat : pattern -> pattern -> bool
val le_pats : pattern list -> pattern list -> bool

(* Exported compatibility, abstracted over constructor equality *)
module Compat : sig
  type eq_cd = constructor_description -> constructor_description -> bool
  val compat : equal_cd:eq_cd -> pattern -> pattern -> bool
  val compats : equal_cd:eq_cd -> pattern list -> pattern list -> bool
end

exception Empty
val lub : pattern -> pattern -> pattern
val lubs : pattern list -> pattern list -> pattern list

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Those two functions recombine one pattern and its arguments:
   For instance:
     (_,_)::p1::p2::rem -> (p1, p2)::rem
   The second one will replace mutable arguments by '_'
*)
val set_args_erase_mutable : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
  pattern -> constructor_tag list -> constructor_description list
val ppat_of_type :
  Env.t ->
  type_expr ->
  Parsetree.pattern
  * (string, constructor_description) Hashtbl.t
  * (string, label_description) Hashtbl.t

val pressure_variants : Env.t -> pattern list -> unit
val check_partial_gadt :
  ?partial_match_warning_hint:string ->
  ((string, constructor_description) Hashtbl.t ->
  (string, label_description) Hashtbl.t ->
  Parsetree.pattern ->
  pattern option) ->
  Location.t ->
  case list ->
  partial
val check_unused :
  ((string, constructor_description) Hashtbl.t ->
  (string, label_description) Hashtbl.t ->
  Parsetree.pattern ->
  pattern option) ->
  case list ->
  unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool

val inactive : partial:partial -> pattern -> bool
(** An inactive pattern is a pattern, matching against which can be duplicated, erased or
    delayed without change in observable behavior of the program.  Patterns containing
    (lazy _) subpatterns or reads of mutable fields are active. *)

(* Ambiguous bindings *)
val check_ambiguous_bindings : case list -> unit

(* The tag used for open polymorphic variant types *)
val some_other_tag : label
