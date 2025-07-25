(* Copyright (C) 2020 - Hongbo Zhang, Authors of ReScript
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

 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type dep_payload = {package_specs: Bsb_package_specs.t; jsx: Bsb_jsx.t}

type t = Toplevel | Dependency of dep_payload
(* This package specs comes from the toplevel to
   override the current settings
*)

let encode_no_nl (x : t) =
  match x with
  | Toplevel -> "0"
  | Dependency x ->
    "1"
    ^ Bsb_package_specs.package_flag_of_package_specs x.package_specs
        ~dirname:"."
    ^ Bsb_jsx.encode_no_nl x.jsx
