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

type spec =
  | Commonjs of {in_source: bool; suffix: string; emit_dts: bool}
  | Esmodule of {in_source: bool; suffix: string; emit_dts: bool}
  | Typescript of {in_source: bool; suffix: string}
      (** TypeScript has inline types, no need for emit_dts *)

let in_source = function
  | Commonjs {in_source; _}
  | Esmodule {in_source; _}
  | Typescript {in_source; _} ->
    in_source

let suffix = function
  | Commonjs {suffix; _} | Esmodule {suffix; _} | Typescript {suffix; _} ->
    suffix

let emit_dts = function
  | Commonjs {emit_dts; _} | Esmodule {emit_dts; _} -> emit_dts
  | Typescript _ -> false

let is_typescript = function
  | Typescript _ -> true
  | Commonjs _ | Esmodule _ -> false

let module_system = function
  | Commonjs _ -> Ext_module_system.Commonjs
  | Esmodule _ -> Ext_module_system.Esmodule
  | Typescript _ -> Ext_module_system.Typescript

let format_name = function
  | Commonjs _ -> Literals.commonjs
  | Esmodule _ -> Literals.esmodule
  | Typescript _ -> Literals.typescript

type t = spec list

let cmp s1 s2 =
  let v = compare (module_system s1) (module_system s2) in
  if v <> 0 then v
  else
    let v = compare (in_source s1) (in_source s2) in
    if v <> 0 then v
    else
      let v = compare (suffix s1) (suffix s2) in
      if v <> 0 then v else compare (emit_dts s1) (emit_dts s2)

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
