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

type t = Parsetree.pattern

let is_unit_cont ~yes ~no (p : t) =
  match p with
  | {ppat_desc = Ppat_construct ({txt = Lident "()"}, None)} -> yes
  | _ -> no

(** [arity_of_fun pat e] tells the arity of 
    expression [fun pat -> e]
*)
let arity_of_fun (pat : Parsetree.pattern) (e : Parsetree.expression) =
  let rec aux (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_fun {rhs = e} -> 1 + aux e (*FIXME error on optional*)
    (* | Pexp_fun _
       -> Location.raise_errorf
           ~loc:e.pexp_loc "Label is not allowed in JS object" *)
    | _ -> 0
  in
  is_unit_cont ~yes:0 ~no:1 pat + aux e

let rec labels_of_fun (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_fun {arg_label = l; rhs = e} -> l :: labels_of_fun e
  | _ -> []

let rec is_single_variable_pattern_conservative (p : t) =
  match p.ppat_desc with
  | Parsetree.Ppat_any -> Some ""
  | Parsetree.Ppat_var s -> Some s.txt
  | Parsetree.Ppat_alias (p, s) ->
    (* Check more complex patterns is needed or not*)
    if is_single_variable_pattern_conservative p <> None then Some s.txt
    else None
  | Parsetree.Ppat_constraint (p, _) ->
    is_single_variable_pattern_conservative p
  | _ -> None
