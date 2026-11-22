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
let flattern_tuple_pattern_vb (self : Ast_mapper.mapper)
    (vb : Parsetree.value_binding) (acc : Parsetree.value_binding list) :
    Parsetree.value_binding list =
  let pvb_pat = self.pat self vb.pvb_pat in
  let pvb_constraint =
    match vb.pvb_constraint with
    | None -> None
    | Some {pvc_newtypes; pvc_type} as option ->
      (* The type before the names, as the record this used to build
         evaluated them *)
      let pvc_type' = self.typ self pvc_type in
      let pvc_newtypes' =
        Ext_list.map_sharing pvc_newtypes (fun (name : string Asttypes.loc) ->
            let loc = self.location self name.loc in
            if loc == name.loc then name else {name with loc})
      in
      if pvc_type' == pvc_type && pvc_newtypes' == pvc_newtypes then option
      else Some {pvc_newtypes = pvc_newtypes'; pvc_type = pvc_type'}
  in
  let pvb_expr = self.expr self vb.pvb_expr in
  let pvb_attributes = self.attributes self vb.pvb_attributes in
  let pvb_jsx_component = vb.pvb_jsx_component in
  (* The binding with its mapped parts, [vb] itself when none changed *)
  let mapped_binding () : Parsetree.value_binding =
    if
      pvb_pat == vb.pvb_pat
      && pvb_constraint == vb.pvb_constraint
      && pvb_expr == vb.pvb_expr
      && pvb_attributes == vb.pvb_attributes
    then vb
    else
      {
        pvb_pat;
        pvb_expr;
        pvb_constraint;
        pvb_jsx_component;
        pvb_loc = vb.pvb_loc;
        pvb_attributes;
      }
  in
  let pvb_expr_desc = (Ast_payload.unwrap_braces pvb_expr).pexp_desc in
  match (pvb_constraint, pvb_pat.ppat_desc, pvb_expr_desc) with
  | Some _, _, _ -> mapped_binding () :: acc
  | None, Ppat_tuple xs, _ when List.for_all is_simple_pattern xs -> (
    match Ast_open_cxt.destruct_open_tuple pvb_expr [] with
    | Some (wholes, es, tuple_attributes)
      when List.for_all is_simple_pattern xs && List.compare_lengths es xs = 0
      ->
      Ast_checks.warn_discarded_unused_attributes tuple_attributes;
      (* will be dropped*)
      List.fold_right2
        (fun pat exp acc ->
          {
            Parsetree.pvb_pat = pat;
            pvb_expr = Ast_open_cxt.restore_exp exp wholes;
            pvb_constraint = None;
            pvb_jsx_component;
            pvb_attributes;
            pvb_loc = vb.pvb_loc;
          }
          :: acc)
        xs es acc
    | _ -> mapped_binding () :: acc)
  | None, Ppat_record (_, _, Some rest), Pexp_pack {pmod_desc = Pmod_ident _} ->
    Loc_report.raise_errorf ~loc:rest.rest_loc
      "Record rest patterns are not supported when destructuring modules. Bind \
       the module fields explicitly."
  | None, Ppat_record (lid_pats, _, None), Pexp_pack {pmod_desc = Pmod_ident id}
    ->
    Ext_list.map_append lid_pats acc (fun {lid; x = pat} ->
        match lid.txt with
        | Lident s ->
          {
            Parsetree.pvb_pat = pat;
            pvb_expr =
              Ast_helper.Exp.ident ~loc:lid.loc
                {lid with txt = Ldot (id.txt, s)};
            pvb_constraint = None;
            pvb_jsx_component = None;
            pvb_attributes = [];
            pvb_loc = pat.ppat_loc;
          }
        | _ ->
          Loc_report.raise_errorf ~loc:lid.loc
            "Not supported pattern match on modules")
  | _ -> mapped_binding () :: acc

(* The bindings are mapped last to first, as [Ext_list.fold_right] did. A list
   whose bindings all come back unchanged is returned as it is. *)
let rec value_bindings_mapper (self : Ast_mapper.mapper)
    (vbs : Parsetree.value_binding list) =
  match vbs with
  | [] -> vbs
  | vb :: rest -> (
    let rest' = value_bindings_mapper self rest in
    match flattern_tuple_pattern_vb self vb rest' with
    | vb' :: tail when vb' == vb && tail == rest' && rest' == rest -> vbs
    | mapped -> mapped)
