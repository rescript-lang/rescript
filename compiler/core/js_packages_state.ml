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

type state = {
  mutable packages_info: Js_packages_info.t;
  mutable making_runtime: bool;
}

(* Package and output specifications belong to a compiler request. Keep the
   record in this module because its type depends on Js_packages_info. *)
let fresh () = {packages_info = Js_packages_info.empty; making_runtime = false}

let key = Domain.DLS.new_key fresh
let current () = Domain.DLS.get key

let with_fresh action =
  let previous = current () in
  Domain.DLS.set key (fresh ());
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)

let set_package_name name =
  let state = current () in
  if Js_packages_info.is_empty state.packages_info then
    state.packages_info <- Js_packages_info.from_name name
  else if not state.making_runtime then
    Bsc_args.bad_arg "duplicated flag for -bs-package-name"

let make_runtime () : unit =
  let state = current () in
  state.making_runtime <- true;
  state.packages_info <- Js_packages_info.runtime_package_specs

let set_package_map module_name =
  (* set_package_name name ;
     let module_name = Ext_namespace.namespace_of_package_name name  in *)
  (Clflags.current ()).dont_record_crc_unit := Some module_name;
  (Clflags.current ()).open_modules :=
    module_name :: !((Clflags.current ()).open_modules)

let update_npm_package_path s =
  let state = current () in
  state.packages_info <-
    Js_packages_info.add_npm_package_path state.packages_info s

let get_packages_info () = (current ()).packages_info

let reset () =
  let state = current () in
  state.packages_info <- Js_packages_info.empty;
  state.making_runtime <- false
