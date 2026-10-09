(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things. *)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Set = sig
  module T : Set.OrderedType
  include Set.S with type elt = T.t and type t = Set.Make(T).t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type Map = sig
  module T : Map.OrderedType
  include Map.S with type key = T.t and type 'a t = 'a Map.Make(T).t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val of_list : (key * 'a) list -> 'a t

  val rename : key t -> key -> key
  val keys : 'a t -> Set.Make(T).t
  val data : 'a t -> 'a list
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Tbl = sig
  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S with type key = T.t and type 'a t = 'a Hashtbl.Make(T).t

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t

  val to_map : 'a t -> 'a Map.Make(T).t
  val of_map : 'a Map.Make(T).t -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T
  module Tbl : Tbl with module T := T
end

module Make (T : Thing) : S with type t := T.t
