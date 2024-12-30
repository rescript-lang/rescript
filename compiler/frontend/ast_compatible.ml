(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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

type attrs = Parsetree.attribute list

open Parsetree

let default_loc = Location.none

let arrow ?loc ?attrs ~arity a b =
  Ast_helper.Typ.arrow ?loc ?attrs ~arity Nolabel a b

let apply_simple ?(loc = default_loc) ?(attrs = []) (fn : expression)
    (args : expression list) : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc =
      Pexp_apply (fn, Ext_list.map args (fun x -> (Asttypes.Nolabel, x)));
  }

let app1 ?(loc = default_loc) ?(attrs = []) fn arg1 : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc = Pexp_apply (fn, [(Nolabel, arg1)]);
  }

let app2 ?(loc = default_loc) ?(attrs = []) fn arg1 arg2 : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc = Pexp_apply (fn, [(Nolabel, arg1); (Nolabel, arg2)]);
  }

let app3 ?(loc = default_loc) ?(attrs = []) fn arg1 arg2 arg3 : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc =
      Pexp_apply (fn, [(Nolabel, arg1); (Nolabel, arg2); (Nolabel, arg3)]);
  }

let fun_ ?(loc = default_loc) ?(attrs = []) ~arity pat exp =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc =
      Pexp_fun
        {arg_label = Nolabel; default = None; lhs = pat; rhs = exp; arity};
  }

let const_exp_string ?(loc = default_loc) ?(attrs = []) ?delimiter (s : string)
    : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc = Pexp_constant (Pconst_string (s, delimiter));
  }

let const_exp_int ?(loc = default_loc) ?(attrs = []) (s : int) : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc = Pexp_constant (Pconst_integer (string_of_int s, None));
  }

let apply_labels ?(loc = default_loc) ?(attrs = []) fn
    (args : (string * expression) list) : expression =
  {
    pexp_loc = loc;
    pexp_attributes = attrs;
    pexp_desc =
      Pexp_apply (fn, Ext_list.map args (fun (l, a) -> (Asttypes.Labelled l, a)));
  }

let label_arrow ?(loc = default_loc) ?(attrs = []) ~arity s a b : core_type =
  {
    ptyp_desc = Ptyp_arrow (Asttypes.Labelled s, a, b, arity);
    ptyp_loc = loc;
    ptyp_attributes = attrs;
  }

let opt_arrow ?(loc = default_loc) ?(attrs = []) ~arity s a b : core_type =
  {
    ptyp_desc = Ptyp_arrow (Asttypes.Optional s, a, b, arity);
    ptyp_loc = loc;
    ptyp_attributes = attrs;
  }

let rec_type_str ?(loc = default_loc) rf tds : structure_item =
  {pstr_loc = loc; pstr_desc = Pstr_type (rf, tds)}

let rec_type_sig ?(loc = default_loc) rf tds : signature_item =
  {psig_loc = loc; psig_desc = Psig_type (rf, tds)}

(* FIXME: need address migration of `[@nonrec]` attributes in older ocaml *)
(* let nonrec_type_sig ?(loc=default_loc)  tds : signature_item =
   {
    psig_loc = loc;
    psig_desc = Psig_type (
      Nonrecursive,
      tds)
   } *)

let const_exp_int_list_as_array xs =
  Ast_helper.Exp.array (Ext_list.map xs (fun x -> const_exp_int x))

(* let const_exp_string_list_as_array xs =
   Ast_helper.Exp.array
   (Ext_list.map xs (fun x -> const_exp_string x ) ) *)

type object_field = Parsetree.object_field

let object_field l attrs ty = Parsetree.Otag (l, attrs, ty)

type args = (Asttypes.arg_label * Parsetree.expression) list
