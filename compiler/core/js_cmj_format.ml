(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2016 - Hongbo Zhang, Authors of ReScript
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

type arity = Single of Lam_arity.t | Submodule of Lam_arity.t array

(* TODO: add a magic number *)
type cmj_value = {
  arity: arity;
  persistent_closed_lambda: Lam.t option;
      (** Either constant or closed functor *)
}

type effect_ = string option

let single_na = Single Lam_arity.na

type keyed_cmj_value = {
  name: string;
  arity: arity;
  persistent_closed_lambda: Lam.t option;
}

type keyed_cmj_values = keyed_cmj_value array

type t = {
  values: keyed_cmj_values;
  pure: bool;
  package_spec: Js_packages_info.t;
  case: Ext_js_file_kind.case;
}

let make ~(values : cmj_value Map_string.t) ~effect_ ~package_spec ~case : t =
  {
    values =
      Map_string.to_sorted_array_with_f values (fun k v ->
          {
            name = k;
            arity = v.arity;
            persistent_closed_lambda = v.persistent_closed_lambda;
          });
    pure = effect_ = None;
    package_spec;
    case;
  }

(* Serialization .. *)
let from_file name : t =
  let ic = open_in_bin name in
  let _digest = Digest.input ic in
  let v : t = input_value ic in
  close_in ic;
  v

let from_file_with_digest name : t * Digest.t =
  let ic = open_in_bin name in
  let digest = Digest.input ic in
  let v : t = input_value ic in
  close_in ic;
  (v, digest)

let from_string s : t = Marshal.from_string s Ext_digest.length

let for_sure_not_changed (name : string) (header : string) =
  if Sys.file_exists name then (
    let ic = open_in_bin name in
    let holder = really_input_string ic Ext_digest.length in
    close_in ic;
    holder = header)
  else false

(* This may cause some build system always rebuild
   maybe should not be turned on by default
*)
let to_file name ~check_exists (v : t) =
  let s = Marshal.to_string v [] in
  let cur_digest = Digest.string s in
  let header = cur_digest in
  if not (check_exists && for_sure_not_changed name header) then (
    let oc = open_out_bin name in
    output_string oc header;
    output_string oc s;
    close_out oc)

let key_comp (a : string) b = Map_string.compare_key a b.name

let not_found key =
  {name = key; arity = single_na; persistent_closed_lambda = None}

let get_result mid_val =
  match mid_val.persistent_closed_lambda with
  | Some
      (Lconst
         (Const_js_null | Const_js_undefined _ | Const_js_true | Const_js_false))
  | None ->
    mid_val
  | Some _ ->
    if !Js_config.cross_module_inline then mid_val
    else {mid_val with persistent_closed_lambda = None}

let rec binary_search_aux arr lo hi (key : string) =
  let mid = (lo + hi) / 2 in
  let mid_val = Array.unsafe_get arr mid in
  let c = key_comp key mid_val in
  if c = 0 then get_result mid_val
  else if c < 0 then
    (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then
      let lo_val = Array.unsafe_get arr lo in
      if lo_val.name = key then get_result lo_val else not_found key
    else binary_search_aux arr lo mid key
  else if
    (*  a[lo] =< a[mid] < key <= a[hi] *)
    lo = mid
  then
    let hi_val = Array.unsafe_get arr hi in
    if hi_val.name = key then get_result hi_val else not_found key
  else binary_search_aux arr mid hi key

let binary_search (sorted : keyed_cmj_values) (key : string) : keyed_cmj_value =
  let len = Array.length sorted in
  if len = 0 then not_found key
  else
    let lo = Array.unsafe_get sorted 0 in
    let c = key_comp key lo in
    if c < 0 then not_found key
    else
      let hi = Array.unsafe_get sorted (len - 1) in
      let c2 = key_comp key hi in
      if c2 > 0 then not_found key else binary_search_aux sorted 0 (len - 1) key

(* FIXME: better error message when ocamldep
   get self-cycle
*)
let query_by_name (cmj_table : t) name : keyed_cmj_value =
  let values = cmj_table.values in
  binary_search values name

type path = string

type cmj_load_info = {
  cmj_table: t;
  package_path: path;
      (*
    Note it is the package path we want 
    for ES6_global module spec
    Maybe we can employ package map in the future
  *)
}
