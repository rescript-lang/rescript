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

(** Extension to standard library [Pervavives] module, safe to open 
*)

external reraise : exn -> 'a = "%raise"

val finally : 'a -> clean:('a -> unit) -> ('a -> 'b) -> 'b

(* val try_it : (unit -> 'a) ->  unit  *)

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val max_int : int -> int -> int

val min_int : int -> int -> int

val max_int_option : int option -> int option -> int option

(* external id : 'a -> 'a = "%identity" *)

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
*)
(* val hash_variant : string -> int *)

(* val todo : string -> 'a *)

val nat_of_string_exn : string -> int

val parse_nat_of_string : string -> int ref -> int
