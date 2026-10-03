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

module Make_map (T : Thing) = struct
  include Map.Make (T)

  let filter_map f t =
    fold
      (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map)
      t empty

  let of_list l = List.fold_left (fun map (id, v) -> add id v map) empty l

  let rename m v = try find v m with Not_found -> v

  let print f ppf s =
    let elts ppf s =
      iter (fun id v -> Format.fprintf ppf "@ (@[%a@ %a@])" T.print id f v) s
    in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  module T_set = Set.Make (T)

  let keys map = fold (fun k _ set -> T_set.add k set) map T_set.empty

  let data t = List.map snd (bindings t)
end

module Make_set (T : Thing) = struct
  include Set.Make (T)

  let output oc s =
    Printf.fprintf oc " ( ";
    iter (fun v -> Printf.fprintf oc "%a " T.output v) s;
    Printf.fprintf oc ")"

  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let of_list l =
    match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))
end

module Make_tbl (T : Thing) = struct
  include Hashtbl.Make (T)

  module T_map = Make_map (T)

  let to_list t = fold (fun key datum elts -> (key, datum) :: elts) t []

  let of_list elts =
    let t = create 42 in
    List.iter (fun (key, datum) -> add t key datum) elts;
    t

  let to_map v = fold T_map.add v T_map.empty

  let of_map m =
    let t = create (T_map.cardinal m) in
    T_map.iter (fun k v -> add t k v) m;
    t

  let map t f = of_map (T_map.map f (to_map t))
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T
  module Tbl : Tbl with module T := T
end

module Make (T : Thing) = struct
  module T = T
  include T

  module Set = Make_set (T)
  module Map = Make_map (T)
  module Tbl = Make_tbl (T)
end
