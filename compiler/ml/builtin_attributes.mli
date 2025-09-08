(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Support for some of the builtin attributes:

   ocaml.deprecated
   ocaml.error
   ocaml.ppwarning
   ocaml.warning
   ocaml.warnerror
   ocaml.explicit_arity (for camlp4/camlp5)
   ocaml.warn_on_literal_pattern
   ocaml.deprecated_mutable
   ocaml.immediate
   ocaml.boxed / ocaml.unboxed
*)

val check_deprecated : Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_inclusion :
  def:Location.t ->
  use:Location.t ->
  Location.t ->
  Parsetree.attributes ->
  Parsetree.attributes ->
  string ->
  unit
val deprecated_of_attrs : Parsetree.attributes -> string option
val deprecated_of_sig : Parsetree.signature -> string option
val deprecated_of_str : Parsetree.structure -> string option

val check_deprecated_mutable :
  Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_mutable_inclusion :
  def:Location.t ->
  use:Location.t ->
  Location.t ->
  Parsetree.attributes ->
  Parsetree.attributes ->
  string ->
  unit

val check_bs_attributes_inclusion :
  (Parsetree.attributes ->
  Parsetree.attributes ->
  string ->
  (string * string) option)
  ref

val check_duplicated_labels :
  (Parsetree.label_declaration list -> string Asttypes.loc option) ref
val error_of_extension : Parsetree.extension -> Location.error

val warning_attribute : ?ppwarning:bool -> Parsetree.attribute -> unit
(** Apply warning settings from the specified attribute.
      "ocaml.warning"/"ocaml.warnerror" (and variants without the prefix)
      are processed and other attributes are ignored.

      Also implement ocaml.ppwarning (unless ~ppwarning:false is
      passed).
  *)

val warning_scope :
  ?ppwarning:bool -> Parsetree.attributes -> (unit -> 'a) -> 'a
(** Execute a function in a new scope for warning settings.  This
      means that the effect of any call to [warning_attribute] during
      the execution of this function will be discarded after
      execution.

      The function also takes a list of attributes which are processed
      with [warning_attribute] in the fresh scope before the function
      is executed.
  *)

val warn_on_literal_pattern : Parsetree.attributes -> bool
val explicit_arity : Parsetree.attributes -> bool

val immediate : Parsetree.attributes -> bool

val has_unboxed : Parsetree.attributes -> bool
val has_boxed : Parsetree.attributes -> bool
