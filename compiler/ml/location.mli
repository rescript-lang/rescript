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

(** Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(** Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val input_name : string ref
val set_input_name : string -> unit

val get_pos_info : Lexing.position -> string * int * int (* file, line, char *)
val print_loc : formatter -> t -> unit

val prerr_warning : t -> Warnings.t -> unit

val warning_printer : (t -> formatter -> Warnings.t -> unit) ref
(** Hook for intercepting warnings. *)

val formatter_for_warnings : formatter ref

val default_warning_printer : t -> formatter -> Warnings.t -> unit
(** Original warning printer for use in hooks. *)

type 'a loc = {txt: 'a; loc: t}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc

val print_filename : formatter -> string -> unit

val show_filename : string -> string

(** Support for located errors *)

type error = {
  loc: t;
  msg: string;
  sub: error list;
  if_highlight: string; (* alternative message if locations are highlighted *)
}

exception Already_displayed_error
exception Error of error

val error : ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error

val errorf :
  ?loc:t ->
  ?sub:error list ->
  ?if_highlight:string ->
  ('a, Format.formatter, unit, error) format4 ->
  'a

val raise_errorf :
  ?loc:t ->
  ?sub:error list ->
  ?if_highlight:string ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

val error_of_printer : t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file : (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn : exn -> [`Ok of error | `Already_displayed] option

val register_error_of_exn : (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). *)

val report_error :
  ?custom_intro:string option ->
  ?src:string option ->
  formatter ->
  error ->
  unit

val error_reporter :
  (?custom_intro:string option ->
  ?src:string option ->
  formatter ->
  error ->
  unit)
  ref
(** Hook for intercepting error reports. *)

val default_error_reporter :
  ?custom_intro:string option ->
  ?src:string option ->
  formatter ->
  error ->
  unit
(** Original error reporter for use in hooks. *)

val report_exception : formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)

val deprecated :
  ?can_be_automigrated:bool -> ?def:t -> ?use:t -> t -> string -> unit

val map_loc : ('a -> 'b) -> 'a loc -> 'b loc
