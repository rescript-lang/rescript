(* Copyright (C) 2020 - Present Hongbo Zhang, Authors of ReScript
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

(* Invariant: the same as encoding Map_string.compare_key  *)
let cmp = Ext_string.compare

let rec binary_search_aux (arr : string array) (lo : int) (hi : int)
    (key : string) : _ option =
  let mid = (lo + hi) / 2 in
  let mid_val = Array.unsafe_get arr mid in
  let c = cmp key mid_val in
  if c = 0 then Some mid
  else if c < 0 then
    (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then
      let lo_val = Array.unsafe_get arr lo in
      if lo_val = key then Some lo else None
    else binary_search_aux arr lo mid key
  else if
    (*  a[lo] =< a[mid] < key <= a[hi] *)
    lo = mid
  then
    let hi_val = Array.unsafe_get arr hi in
    if hi_val = key then Some hi else None
  else binary_search_aux arr mid hi key

let find_sorted sorted key : int option =
  let len = Array.length sorted in
  if len = 0 then None
  else
    let lo = Array.unsafe_get sorted 0 in
    let c = cmp key lo in
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in
      let c2 = cmp key hi in
      if c2 > 0 then None else binary_search_aux sorted 0 (len - 1) key

let rec binary_search_assoc (arr : (string * _) array) (lo : int) (hi : int)
    (key : string) : _ option =
  let mid = (lo + hi) / 2 in
  let mid_val = Array.unsafe_get arr mid in
  let c = cmp key (fst mid_val) in
  if c = 0 then Some (snd mid_val)
  else if c < 0 then
    (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then
      let lo_val = Array.unsafe_get arr lo in
      if fst lo_val = key then Some (snd lo_val) else None
    else binary_search_assoc arr lo mid key
  else if
    (*  a[lo] =< a[mid] < key <= a[hi] *)
    lo = mid
  then
    let hi_val = Array.unsafe_get arr hi in
    if fst hi_val = key then Some (snd hi_val) else None
  else binary_search_assoc arr mid hi key

let find_sorted_assoc (type a) (sorted : (string * a) array) (key : string) :
    a option =
  let len = Array.length sorted in
  if len = 0 then None
  else
    let lo = Array.unsafe_get sorted 0 in
    let c = cmp key (fst lo) in
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in
      let c2 = cmp key (fst hi) in
      if c2 > 0 then None else binary_search_assoc sorted 0 (len - 1) key
