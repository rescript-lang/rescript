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

type lam = Lambda.lambda

type hash_names = (int * string) list

type input = (int * (string * lam)) list

type output = (hash_names * lam) list

module Coll = Hash.Make (struct
  type t = lam

  let equal = Stdlib.( = )

  let hash = Hashtbl.hash
end)

type value = {stamp: int; hash_names_act: hash_names * lam}

let convert (xs : input) : output =
  let coll = Coll.create 63 in
  let os : value list ref = ref [] in
  xs
  |> List.iteri (fun i (hash, (name, act)) ->
         match Lambda.make_key act with
         | None ->
           os := {stamp = i; hash_names_act = ([(hash, name)], act)} :: !os
         | Some key ->
           Coll.add_or_update coll key
             ~update:(fun ({hash_names_act = hash_names, act} as acc) ->
               {acc with hash_names_act = ((hash, name) :: hash_names, act)})
             {hash_names_act = ([(hash, name)], act); stamp = i});
  let result = Coll.to_list coll (fun _ value -> value) @ !os in
  Ext_list.sort_via_arrayf result
    (fun x y -> compare x.stamp y.stamp)
    (fun x -> x.hash_names_act)

let or_list (arg : lam) (hash_names : (int * string) list) =
  match hash_names with
  | (hash, name) :: rest ->
    let init : lam =
      Lprim
        ( Pintcomp Ceq,
          [arg; Lconst (Const_pointer (hash, Pt_variant {name}))],
          Location.none )
    in
    Ext_list.fold_left rest init (fun acc (hash, name) ->
        Lambda.Lprim
          ( Psequor,
            [
              acc;
              Lprim
                ( Pintcomp Ceq,
                  [arg; Lconst (Const_pointer (hash, Pt_variant {name}))],
                  Location.none );
            ],
            Location.none ))
  | _ -> assert false

let make_test_sequence_variant_constant (fail : lam option) (arg : lam)
    (int_lambda_list : (int * (string * lam)) list) : lam =
  let int_lambda_list : ((int * string) list * lam) list =
    convert int_lambda_list
  in
  match (int_lambda_list, fail) with
  | (_, act) :: rest, None | rest, Some act ->
    Ext_list.fold_right rest act (fun (hash_names, act1) acc ->
        let predicate : lam = or_list arg hash_names in
        Lifthenelse (predicate, act1, acc))
  | [], None -> assert false

let call_switcher_variant_constant (_loc : Location.t) (fail : lam option)
    (arg : lam) (int_lambda_list : (int * (string * lam)) list)
    (_names : Ast_untagged_variants.switch_names option) =
  let int_lambda_list = convert int_lambda_list in
  match (int_lambda_list, fail) with
  | (_, act) :: rest, None | rest, Some act ->
    Ext_list.fold_right rest act (fun (hash_names, act1) acc ->
        let predicate = or_list arg hash_names in
        Lifthenelse (predicate, act1, acc))
  | [], None -> assert false

let call_switcher_variant_constr (loc : Location.t) (fail : lam option)
    (arg : lam) int_lambda_list
    (names : Ast_untagged_variants.switch_names option) : lam =
  let v = Ident.create "variant" in
  Llet
    ( Alias,
      Pgenval,
      v,
      Lprim (Pfield (0, Fld_poly_var_tag), [arg], loc),
      call_switcher_variant_constant loc fail (Lvar v) int_lambda_list names )
