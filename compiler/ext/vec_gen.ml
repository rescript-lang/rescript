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

module type ResizeType = sig
  type t

  val null : t
  (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end

module type S = sig
  type elt

  type t

  val length : t -> int

  val compact : t -> unit

  val singleton : elt -> t

  val empty : unit -> t

  val make : int -> t

  val init : int -> (int -> elt) -> t

  val is_empty : t -> bool

  val of_sub_array : elt array -> int -> int -> t

  val unsafe_internal_array : t -> elt array
  (** Exposed for some APIs which only take array as input, 
      when exposed   
  *)

  val reserve : t -> int -> unit

  val push : t -> elt -> unit

  val delete : t -> int -> unit

  val pop : t -> unit

  val get_last_and_pop : t -> elt

  val delete_range : t -> int -> int -> unit

  val get_and_delete_range : t -> int -> int -> t

  val clear : t -> unit

  val reset : t -> unit

  val to_list : t -> elt list

  val of_list : elt list -> t

  val to_array : t -> elt array

  val of_array : elt array -> t

  val copy : t -> t

  val reverse_in_place : t -> unit

  val iter : t -> (elt -> unit) -> unit

  val iteri : t -> (int -> elt -> unit) -> unit

  val iter_range : t -> from:int -> to_:int -> (elt -> unit) -> unit

  val iteri_range : t -> from:int -> to_:int -> (int -> elt -> unit) -> unit

  val map : (elt -> elt) -> t -> t

  val mapi : (int -> elt -> elt) -> t -> t

  val map_into_array : (elt -> 'f) -> t -> 'f array

  val map_into_list : (elt -> 'f) -> t -> 'f list

  val fold_left : ('f -> elt -> 'f) -> 'f -> t -> 'f

  val fold_right : (elt -> 'g -> 'g) -> t -> 'g -> 'g

  val filter : (elt -> bool) -> t -> t

  val inplace_filter : (elt -> bool) -> t -> unit

  val inplace_filter_with :
    (elt -> bool) -> cb_no:(elt -> 'a -> 'a) -> 'a -> t -> 'a

  val inplace_filter_from : int -> (elt -> bool) -> t -> unit

  val equal : (elt -> elt -> bool) -> t -> t -> bool

  val get : t -> int -> elt

  val unsafe_get : t -> int -> elt

  val last : t -> elt

  val capacity : t -> int

  val exists : (elt -> bool) -> t -> bool

  val sub : t -> int -> int -> t
end
