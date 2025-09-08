(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 -  Hongbo Zhang, Authors of ReScript
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

let local_external_apply loc ?(pval_attributes = []) ~(pval_prim : string list)
    ~(pval_type : Parsetree.core_type) ?(local_module_name = "J")
    ?(local_fun_name = "unsafe_expr") (args : Parsetree.expression list) :
    Parsetree.expression_desc =
  Pexp_letmodule
    ( {txt = local_module_name; loc},
      {
        pmod_desc =
          Pmod_structure
            [
              {
                pstr_desc =
                  Pstr_primitive
                    {
                      pval_name = {txt = local_fun_name; loc};
                      pval_type;
                      pval_loc = loc;
                      pval_prim;
                      pval_attributes;
                    };
                pstr_loc = loc;
              };
            ];
        pmod_loc = loc;
        pmod_attributes = [];
      },
      Ast_compatible.apply_simple
        ({
           pexp_desc =
             Pexp_ident
               {txt = Ldot (Lident local_module_name, local_fun_name); loc};
           pexp_attributes = [];
           pexp_loc = loc;
         }
          : Parsetree.expression)
        args ~loc )

let local_external_obj loc ?(pval_attributes = []) ~pval_prim ~pval_type
    ?(local_module_name = "J") ?(local_fun_name = "unsafe_expr") args :
    Parsetree.expression_desc =
  Pexp_letmodule
    ( {txt = local_module_name; loc},
      {
        pmod_desc =
          Pmod_structure
            [
              {
                pstr_desc =
                  Pstr_primitive
                    {
                      pval_name = {txt = local_fun_name; loc};
                      pval_type;
                      pval_loc = loc;
                      pval_prim;
                      pval_attributes;
                    };
                pstr_loc = loc;
              };
            ];
        pmod_loc = loc;
        pmod_attributes = [];
      },
      Ast_compatible.apply_labels
        ({
           pexp_desc =
             Pexp_ident
               {txt = Ldot (Lident local_module_name, local_fun_name); loc};
           pexp_attributes = [];
           pexp_loc = loc;
         }
          : Parsetree.expression)
        args ~loc )

let local_extern_cont_to_obj loc ?(pval_attributes = []) ~pval_prim ~pval_type
    ?(local_module_name = "J") ?(local_fun_name = "unsafe_expr")
    (cb : Parsetree.expression -> 'a) : Parsetree.expression_desc =
  Pexp_letmodule
    ( {txt = local_module_name; loc},
      {
        pmod_desc =
          Pmod_structure
            [
              {
                pstr_desc =
                  Pstr_primitive
                    {
                      pval_name = {txt = local_fun_name; loc};
                      pval_type;
                      pval_loc = loc;
                      pval_prim;
                      pval_attributes;
                    };
                pstr_loc = loc;
              };
            ];
        pmod_loc = loc;
        pmod_attributes = [];
      },
      cb
        {
          pexp_desc =
            Pexp_ident
              {txt = Ldot (Lident local_module_name, local_fun_name); loc};
          pexp_attributes = [];
          pexp_loc = loc;
        } )
