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

(** Reporting errors and warnings at source locations ({!Location.t}). *)

open Format

val get_input_name : unit -> string
(** The source filename for the current compiler request on this domain. *)

val set_input_name : string -> unit
val print_loc : formatter -> Location.t -> unit

val prerr_warning : Location.t -> Warnings.t -> unit

val warning_printer : (Location.t -> formatter -> Warnings.t -> unit) ref
(** Hook for intercepting warnings. *)

val formatter_for_warnings : formatter ref

val default_warning_printer : Location.t -> formatter -> Warnings.t -> unit
(** Original warning printer for use in hooks. *)

val print_filename : formatter -> string -> unit

val show_filename : string -> string

(** Support for located errors *)

type error = {
  loc: Location.t;
  msg: string;
  sub: error list;
  if_highlight: string; (* alternative message if locations are highlighted *)
}

exception Already_displayed_error
exception Error of error

val error :
  ?loc:Location.t -> ?sub:error list -> ?if_highlight:string -> string -> error

val errorf : loc:Location.t -> ('a, Format.formatter, unit, error) format4 -> 'a

val raise_errorf :
  ?loc:Location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val error_of_printer : Location.t -> (formatter -> 'a -> unit) -> 'a -> error

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

val default_error_reporter :
  ?custom_intro:string option ->
  ?src:string option ->
  formatter ->
  error ->
  unit
[@@live]
(** Original error reporter for use in hooks. Live: called through
    {!error_reporter} with optional arguments that reanalyze cannot trace. *)

val report_exception : formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)

val deprecated :
  ?can_be_automigrated:bool ->
  ?def:Location.t ->
  ?use:Location.t ->
  Location.t ->
  string ->
  unit
