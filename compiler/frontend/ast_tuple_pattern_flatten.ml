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

(* type loc = Location.t

   type exp = Parsetree.expression

   type pat = Parsetree.pattern *)

let rec is_simple_pattern (p : Parsetree.pattern) =
  match p.ppat_desc with
  | Ppat_any -> true
  | Ppat_var _ -> true
  | Ppat_constraint (p, _) -> is_simple_pattern p
  | _ -> false

(*
  [let (a,b) = M.N.(c,d) ]
  =>
  [ let a = M.N.c
    and b = M.N.d ]
*)
let flattern_tuple_pattern_vb (self : Bs_ast_mapper.mapper)
    (vb : Parsetree.value_binding) (acc : Parsetree.value_binding list) :
    Parsetree.value_binding list =
  let pvb_pat = self.pat self vb.pvb_pat in
  let pvb_expr = self.expr self vb.pvb_expr in
  let pvb_attributes = self.attributes self vb.pvb_attributes in
  match (pvb_pat.ppat_desc, pvb_expr.pexp_desc) with
  | Ppat_tuple xs, _ when List.for_all is_simple_pattern xs -> (
    match Ast_open_cxt.destruct_open_tuple pvb_expr [] with
    | Some (wholes, es, tuple_attributes)
      when Ext_list.for_all xs is_simple_pattern && Ext_list.same_length es xs
      ->
      Bs_ast_invariant.warn_discarded_unused_attributes tuple_attributes;
      (* will be dropped*)
      Ext_list.fold_right2 xs es acc (fun pat exp acc ->
          {
            pvb_pat = pat;
            pvb_expr = Ast_open_cxt.restore_exp exp wholes;
            pvb_attributes;
            pvb_loc = vb.pvb_loc;
          }
          :: acc)
    | _ -> {pvb_pat; pvb_expr; pvb_loc = vb.pvb_loc; pvb_attributes} :: acc)
  | Ppat_record (lid_pats, _), Pexp_pack {pmod_desc = Pmod_ident id} ->
    Ext_list.map_append lid_pats acc (fun {lid; x = pat} ->
        match lid.txt with
        | Lident s ->
          {
            pvb_pat = pat;
            pvb_expr =
              Ast_helper.Exp.ident ~loc:lid.loc
                {lid with txt = Ldot (id.txt, s)};
            pvb_attributes = [];
            pvb_loc = pat.ppat_loc;
          }
        | _ ->
          Location.raise_errorf ~loc:lid.loc
            "Not supported pattern match on modules")
  | _ -> {pvb_pat; pvb_expr; pvb_loc = vb.pvb_loc; pvb_attributes} :: acc

let value_bindings_mapper (self : Bs_ast_mapper.mapper)
    (vbs : Parsetree.value_binding list) =
  (* Bs_ast_mapper.default_mapper.value_bindings self  vbs   *)
  Ext_list.fold_right vbs [] (fun vb acc ->
      flattern_tuple_pattern_vb self vb acc)
