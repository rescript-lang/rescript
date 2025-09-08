(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

(* Make it mutable so that we can do
   in-place change without constructing a new one
   -- however, it's a design choice -- to be reviewed later
*)

type immutable_mask =
  | All_immutable_and_no_tail_call
      (** iff not tailcalled 
      if tailcalled, in theory, it does not need change params, 
      for example
      {[
        let rec f  (n : int ref) = 
          if !n > 0 then decr n; print_endline "hi"
      else  f n
      ]}
      in this case, we still create [Immutable_mask], 
      since the inline behavior is slightly different
  *)
  | Immutable_mask of bool array

type t = {
  mutable unbounded: Set_ident.t;
  used_mask: bool array;
  immutable_mask: immutable_mask;
}
(** Invariant: unused param has to be immutable *)

let make ?immutable_mask n =
  {
    unbounded = Set_ident.empty;
    used_mask = Array.make n false;
    immutable_mask =
      (match immutable_mask with
      | Some x -> Immutable_mask x
      | None -> All_immutable_and_no_tail_call);
  }

let no_tailcall x =
  match x.immutable_mask with
  | All_immutable_and_no_tail_call -> []
  | Immutable_mask arr -> Array.to_list arr

let mark_unused t i = t.used_mask.(i) <- true

let get_unused t i = t.used_mask.(i)

(* let get_length t = Array.length t.used_mask *)

(* let to_string env =
   String.concat ","
    (Ext_list.map (Set_ident.elements  env.unbounded )
      (fun id  -> Printf.sprintf "%s/%d" id.name id.stamp)
       ) *)

let get_mutable_params (params : Ident.t list) (x : t) =
  match x.immutable_mask with
  | All_immutable_and_no_tail_call -> []
  | Immutable_mask xs ->
    Ext_list.filter_mapi params (fun p i -> if not xs.(i) then Some p else None)

let get_unbounded t = t.unbounded

let set_unbounded env v =
  (* Ext_log.err "%s -- set @." (to_string env); *)
  (* if Set_ident.is_empty env.bound then *)
  env.unbounded <- v
(* else assert false *)
