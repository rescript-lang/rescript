(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** A utility module used when destructuring parsetree attributes, used for 
    compiling FFI attributes and built-in ppx  *)

type t = Parsetree.payload

type lid = string Asttypes.loc

type action = lid * Parsetree.expression option

val json_literal_outside_external_message : string
val reject_json_literal : loc:Location.t -> 'a
val reject_json_literal_payload : t -> unit

val semantic_string_of_expression : Parsetree.expression -> string option
(** Return the decoded value when the expression is an ordinary string or a
    non-interpolated backquoted string. *)

val string_literal_of_payload : t -> String_literal.string_literal option
(** Return the string payload of an attribute that denotes a name, keeping its
    source spelling. A backquoted payload is given canonical quoted spelling,
    since it is the same name written differently. *)

val semantic_string_of_payload : t -> string option
(** Return the decoded value of an ordinary or non-interpolated backquoted string.
    Other prefixed literals, such as [json], are not semantic strings. *)

val is_single_int : t -> int option

val constructor_tag_of_payload : t -> Parsetree.constructor_tag option
(** The literal denoted by a valid variant-constructor [@as] payload. *)

val raw_as_string_exp_exn :
  kind:Js_raw_info.raw_kind ->
  ?is_function:int option ref ->
  t ->
  Parsetree.expression option
(** Convert %raw into expression *)

val assert_strings : Location.t -> t -> string list

(** as a record or empty 
    it will accept 

    {[ [@@@config ]]}
    or 
    {[ [@@@config no_export ] ]}
    or 
    {[ [@@@config { property  .. } ]]}    
    Note that we only 
    {[
      { flat_property}
    ]}
    below  is not allowed 
    {[
      {M.flat_property}
    ]}
*)

val ident_or_record_as_config : Location.t -> t -> action list

val assert_bool_lit : Parsetree.expression -> bool

val empty : t

val table_dispatch :
  (Parsetree.expression option -> 'a) Map_string.t -> action -> 'a

val unrecognized_config_record : Location.t -> string -> unit
(** Report to the user, as a warning, that the bs-attribute parser is bailing out. (This is to allow
    external ppx, like ppx_deriving, to pick up where the builtin ppx leave off.) *)
