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

val reverse_range : 'a array -> int -> int -> unit
(** Some utilities for {!Array} operations *)

val reverse_in_place : 'a array -> unit

val reverse : 'a array -> 'a array

val reverse_of_list : 'a list -> 'a array

val filter : 'a array -> ('a -> bool) -> 'a array

val filter_map : 'a array -> ('a -> 'b option) -> 'b array

val filter_mapi : 'a array -> (int -> 'a -> 'b option) -> 'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

val to_list_f : 'a array -> ('a -> 'b) -> 'b list

val to_list_map : 'a array -> ('a -> 'b option) -> 'b list

val to_list_map_acc : 'a array -> 'b list -> ('a -> 'b option) -> 'b list

val of_list_map : 'a list -> ('a -> 'b) -> 'b array

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int

type 'a split = No_split | Split of 'a array * 'a array

val find_and_split : 'a array -> ('a -> 'b -> bool) -> 'b -> 'a split

val exists : 'a array -> ('a -> bool) -> bool

val is_empty : 'a array -> bool

val for_all2_no_exn : 'a array -> 'b array -> ('a -> 'b -> bool) -> bool

val for_alli : 'a array -> (int -> 'a -> bool) -> bool

val map : 'a array -> ('a -> 'b) -> 'b array

val iter : 'a array -> ('a -> unit) -> unit

val fold_left : 'b array -> 'a -> ('a -> 'b -> 'a) -> 'a

val get_or : 'a array -> int -> (unit -> 'a) -> 'a
