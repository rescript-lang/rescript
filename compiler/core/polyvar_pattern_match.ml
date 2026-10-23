(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript 
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

type lam = Lambda.t

type names = string list

type input = (int * (string * lam)) list

type output = (names * lam) list

module Coll = Hashtbl.Make (struct
  type t = lam

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash
end)

type value = {stamp: int; names_act: names * lam}

let convert (xs : input) : output =
  let coll = Coll.create 63 in
  let os : value list ref = ref [] in
  xs
  |> List.iteri (fun i (_hash, (name, act)) ->
      match Lambda_traverse.make_key act with
      | None -> os := {stamp = i; names_act = ([name], act)} :: !os
      | Some key -> (
        match Coll.find_opt coll key with
        | Some ({names_act = names, act} as acc) ->
          Coll.replace coll key {acc with names_act = (name :: names, act)}
        | None -> Coll.add coll key {names_act = ([name], act); stamp = i}));
  let result = Coll.fold (fun _ value acc -> value :: acc) coll !os in
  Ext_list.sort_via_arrayf result
    (fun x y -> compare x.stamp y.stamp)
    (fun x -> x.names_act)

let or_list (arg : lam) (names : names) =
  match names with
  | name :: rest ->
    let init : lam =
      Lambda.prim ~primitive:(Pintcomp Ceq)
        ~args:[arg; Lambda.const (Lambda.const_polyvar name)]
        Location.none
    in
    Ext_list.fold_left rest init (fun acc name ->
        Lambda.prim ~primitive:Psequor
          ~args:
            [
              acc;
              Lambda.prim ~primitive:(Pintcomp Ceq)
                ~args:[arg; Lambda.const (Lambda.const_polyvar name)]
                Location.none;
            ]
          Location.none)
  | _ -> assert false

let make_test_sequence_variant_constant (fail : lam option) (arg : lam)
    (int_lambda_list : (int * (string * lam)) list) : lam =
  let names_lambda_list : output = convert int_lambda_list in
  match (names_lambda_list, fail) with
  | (_, act) :: rest, None | rest, Some act ->
    Ext_list.fold_right rest act (fun (names, act1) acc ->
        let predicate : lam = or_list arg names in
        Lambda.if_ predicate act1 acc)
  | [], None -> assert false

(* Both Matching hooks compile constant polymorphic-variant cases to the
   same chain of tag tests. *)
let call_switcher_variant_constant = make_test_sequence_variant_constant

let call_switcher_variant_constr (loc : Location.t) (fail : lam option)
    (arg : lam) int_lambda_list : lam =
  let v = Ident.create "variant" in
  Lambda.let_ Alias v
    (Lambda.prim ~primitive:(Pfield (0, Fld_poly_var_tag)) ~args:[arg] loc)
    (call_switcher_variant_constant fail (Lambda.var v) int_lambda_list)
