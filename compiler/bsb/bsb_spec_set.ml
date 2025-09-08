(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

[@@@warning "+9"]

(* TODO: sync up with {!Js_packages_info.module_system}  *)
type format = Ext_module_system.t

type spec = {format: format; in_source: bool; suffix: string}

type t = spec list

let cmp (s1 : spec) ({format; in_source; suffix} : spec) =
  let v = compare s1.format format in
  if v <> 0 then v
  else
    let v = compare s1.in_source in_source in
    if v <> 0 then v else compare s1.suffix suffix

let empty = []

let rec insert lst piviot =
  match lst with
  | [] -> [piviot]
  | x :: xs ->
    let v = cmp piviot x in
    if v = 0 then lst
    else if v < 0 then piviot :: lst
    else x :: insert xs piviot

let add spec specs =
  match specs with
  | [] -> [spec]
  | [a] ->
    let v = cmp spec a in
    if v < 0 then spec :: specs else if v = 0 then specs else [a; spec]
  | [a; b] ->
    let v = cmp spec a in
    if v < 0 then spec :: specs
    else if v = 0 then specs
    else
      let v1 = cmp spec b in
      if v < 0 then [a; spec; b] else if v1 = 0 then specs else [a; b; spec]
  | _ :: _ :: _ :: _ ->
    (* unlikely to happen *)
    insert specs spec

let singleton x = [x]

let rec fold f t acc =
  match t with
  | [] -> acc
  | x :: xs -> fold f xs (f x acc)

let rec iter f t =
  match t with
  | [] -> ()
  | x :: xs ->
    f x;
    iter f xs
