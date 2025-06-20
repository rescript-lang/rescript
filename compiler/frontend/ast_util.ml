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

let js_property loc obj (name : string) =
  Parsetree.Pexp_send (obj, {loc; txt = name})

let record_as_js_object loc (self : Bs_ast_mapper.mapper)
    (label_exprs : Parsetree.expression Parsetree.record_element list) :
    Parsetree.expression_desc =
  let labels, args, arity =
    Ext_list.fold_right label_exprs ([], [], 0)
      (fun {lid = {txt; loc}; x = e} (labels, args, i) ->
        match txt with
        | Lident x ->
          ( {Asttypes.loc; txt = x} :: labels,
            (x, self.expr self e) :: args,
            i + 1 )
        | Ldot _ | Lapply _ -> Location.raise_errorf ~loc "invalid js label ")
  in
  Ast_external_mk.local_external_obj loc
    ~pval_prim:(Ast_external_process.pval_prim_of_labels labels)
    ~pval_type:(Ast_core_type.from_labels ~loc arity labels)
    args
