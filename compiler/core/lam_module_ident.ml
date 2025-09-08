(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

type t = J.module_id = {id: Ident.t; kind: Js_op.kind; dynamic_import: bool}

let id x = x.id

let of_ml ?(dynamic_import = false) id = {id; kind = Ml; dynamic_import}

let of_runtime id = {id; kind = Runtime; dynamic_import = false}

let name (x : t) : string =
  match x.kind with
  | Ml | Runtime -> x.id.name
  | External {name = v} -> v

module Cmp = struct
  [@@@warning "+9"]
  type nonrec t = t
  let equal (x : t) y =
    match x.kind with
    | External {name = x_kind; default = x_default; _} -> (
      match y.kind with
      | External {name = y_kind; default = y_default; _} ->
        x_kind = (y_kind : string) && x_default = y_default
      | _ -> false)
    | Ml | Runtime -> Ext_ident.equal x.id y.id

  (* #1556
     Note the main difference between [Ml] and [Runtime] is
     that we have more assumptions about [Runtime] module,
     like its purity etc, and its name uniqueues, in the pattern match
     {[
       {Runtime, "caml_int_compare"}
     ]}
     and we could do more optimziations.
     However, here if it is [hit]
     (an Ml module = an Runtime module), which means both exists,
     so adding either does not matter
     if it is not hit, fine
  *)
  let hash (x : t) =
    match x.kind with
    | External {name = x_kind; _} ->
      (* The hash collision is rare? *)
      Bs_hash_stubs.hash_string x_kind
    | Ml | Runtime ->
      let x_id = x.id in
      Bs_hash_stubs.hash_stamp_and_name x_id.stamp x_id.name
end

module Hash = Hash.Make (Cmp)

module Hash_set = Hash_set.Make (Cmp)
