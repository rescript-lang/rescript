(* Copyright (C) 2019 - Present Hongbo Zhang, Authors of ReScript
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

type loc = Location.t

type whole =
  | Let_open of
      (Asttypes.override_flag
      * Longident.t Asttypes.loc
      * loc
      * Parsetree.attributes)

type t = whole list
type exp = Parsetree.expression
type destruct_output = exp list

(**
   destruct such pattern
   {[ A.B.let open C in (a,b)]}
*)
let rec destruct_open_tuple (e : Parsetree.expression) (acc : t) :
    (t * destruct_output * _) option =
  match e.pexp_desc with
  | Pexp_open (flag, lid, cont) ->
    destruct_open_tuple cont
      (Let_open (flag, lid, e.pexp_loc, e.pexp_attributes) :: acc)
  | Pexp_tuple es -> Some (acc, es, e.pexp_attributes)
  | _ -> None

let restore_exp (xs : Parsetree.expression) (qualifiers : t) :
    Parsetree.expression =
  Ext_list.fold_left qualifiers xs (fun x hole ->
      match hole with
      | Let_open (flag, lid, loc, attrs) ->
        ({
           pexp_desc = Pexp_open (flag, lid, x);
           pexp_attributes = attrs;
           pexp_loc = loc;
         }
          : Parsetree.expression))
