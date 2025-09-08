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

 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(*
  The purpose of the default warning set is to make it strict while not annoying the user too much.

  - 4 Fragile pattern matching: matching that will remain complete even if additional constructors are added to one of the variant types matched.
  We turn it off since the following is a common pattern:
   {[
     switch x { | A => .. | _ => false }
   ]}

   - 9 Missing fields in a record pattern.
   Only in some special cases that we need all fields being listed

   - 41 Ambiguous constructor or label name.
     It is turned off since it prevents such cases below:
   {[
     type a = A | B
     type b = A | B | C
   ]}

   - 50 Unexpected documentation comment.

   - 102 Bs_polymorphic_comparison.
*)
(* If you change this, don't forget to adapt docs/docson/build-schema.json as well. *)
let defaults_w = "+a-4-9-20-41-50-102"

let defaults_warn_error = "-a+5+6+101+109"
(*TODO: add +10*)
