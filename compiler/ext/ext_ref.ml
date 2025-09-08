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

let non_exn_protect r v body =
  let old = !r in
  r := v;
  let res = body () in
  r := old;
  res

let protect r v body =
  let old = !r in
  try
    r := v;
    let res = body () in
    r := old;
    res
  with x ->
    r := old;
    raise x

let non_exn_protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in
  r1 := v1;
  r2 := v2;
  let res = body () in
  r1 := old1;
  r2 := old2;
  res

let protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in
  try
    r1 := v1;
    r2 := v2;
    let res = body () in
    r1 := old1;
    r2 := old2;
    res
  with x ->
    r1 := old1;
    r2 := old2;
    raise x

let protect_list rvs body =
  let olds = Ext_list.map rvs (fun (x, _) -> !x) in
  let () = List.iter (fun (x, y) -> x := y) rvs in
  try
    let res = body () in
    List.iter2 (fun (x, _) old -> x := old) rvs olds;
    res
  with e ->
    List.iter2 (fun (x, _) old -> x := old) rvs olds;
    raise e
