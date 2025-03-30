(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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

(* When we design a ppx, we should keep it simple, and also think about
   how it would work with other tools like merlin and ocamldep *)

(**
   1. extension point
   {[
     [%raw{| blabla |}]
   ]}
   will be desugared into
   {[
     let module Js =
     struct unsafe_js : string -> 'a end
     in Js.unsafe_js {| blabla |}
   ]}
   The major benefit is to better error reporting (with locations).
   Otherwise

   {[

     let f u = Js.unsafe_js u
     let _ = f (1 + 2)
   ]}
   And if it is inlined some where
*)

let () =
  Ast_derive_projector.init ();
  Ast_derive_js_mapper.init ()

let succeed attr attrs =
  match attrs with
  | [_] -> ()
  | _ ->
    Used_attributes.mark_used_attribute attr;
    Bs_ast_invariant.warn_discarded_unused_attributes attrs

type mapper = Bs_ast_mapper.mapper

let default_mapper = Bs_ast_mapper.default_mapper
let default_expr_mapper = Bs_ast_mapper.default_mapper.expr
let default_pat_mapper = Bs_ast_mapper.default_mapper.pat

let pat_mapper (self : mapper) (p : Parsetree.pattern) =
  match p.ppat_desc with
  | Ppat_constant (Pconst_integer (s, Some 'l')) ->
    {p with ppat_desc = Ppat_constant (Pconst_integer (s, None))}
  | Ppat_constant (Pconst_string (s, Some delim)) ->
    Ast_utf8_string_interp.transform_pat p s delim
  | _ -> default_pat_mapper self p

(* Unpack requires core_type package for type inference:
   Generate a module type name eg. __Belt_List__*)
let local_module_type_name txt =
  "_"
  ^ (Longident.flatten txt |> List.fold_left (fun ll l -> ll ^ "_" ^ l) "")
  ^ "__"

let expr_mapper ~async_context ~in_function_def (self : mapper)
    (e : Parsetree.expression) =
  let old_in_function_def = !in_function_def in
  in_function_def := false;
  match e.pexp_desc with
  (* Its output should not be rewritten anymore *)
  | Pexp_extension extension ->
    Ast_exp_extension.handle_extension e self extension
  | Pexp_constant (Pconst_string (s, Some delim)) ->
    Ast_utf8_string_interp.transform_exp e s delim
  | Pexp_constant (Pconst_integer (s, Some 'l')) ->
    {e with pexp_desc = Pexp_constant (Pconst_integer (s, None))}
  (* End rewriting *)
  | Pexp_newtype (s, body) ->
    let res = self.expr self body in
    {e with pexp_desc = Pexp_newtype (s, res)}
  | Pexp_fun {arg_label = label; lhs = pat; rhs = body; async} -> (
    match Ast_attributes.process_attributes_rev e.pexp_attributes with
    | Nothing, _ ->
      (* Handle @async x => y => ... is in async context *)
      async_context := (old_in_function_def && !async_context) || async;
      in_function_def := true;
      Ast_async.make_function_async ~async (default_expr_mapper self e)
    | Method _, _ ->
      Location.raise_errorf ~loc:e.pexp_loc
        "%@meth is not supported in function expression"
    | Meth_callback _, pexp_attributes ->
      (* FIXME: does it make sense to have a label for [this] ? *)
      async_context := false;
      {
        e with
        pexp_desc =
          Ast_uncurry_gen.to_method_callback e.pexp_loc self label pat body;
        pexp_attributes;
      })
  | Pexp_apply _ -> Ast_exp_apply.app_exp_mapper e self
  | Pexp_match
      ( b,
        [
          {
            pc_lhs = {ppat_desc = Ppat_construct ({txt = Lident "true"}, None)};
            pc_guard = None;
            pc_rhs = t_exp;
          };
          {
            pc_lhs = {ppat_desc = Ppat_construct ({txt = Lident "false"}, None)};
            pc_guard = None;
            pc_rhs = f_exp;
          };
        ] )
  | Pexp_match
      ( b,
        [
          {
            pc_lhs = {ppat_desc = Ppat_construct ({txt = Lident "false"}, None)};
            pc_guard = None;
            pc_rhs = f_exp;
          };
          {
            pc_lhs = {ppat_desc = Ppat_construct ({txt = Lident "true"}, None)};
            pc_guard = None;
            pc_rhs = t_exp;
          };
        ] ) ->
    default_expr_mapper self
      {e with pexp_desc = Pexp_ifthenelse (b, t_exp, Some f_exp)}
  | Pexp_let
      ( Nonrecursive,
        [
          {
            pvb_pat =
              ( {ppat_desc = Ppat_record _}
              | {ppat_desc = Ppat_alias ({ppat_desc = Ppat_record _}, _)} ) as p;
            pvb_expr;
            pvb_attributes;
            pvb_loc = _;
          };
        ],
        body ) -> (
    match pvb_expr.pexp_desc with
    | Pexp_pack _ -> default_expr_mapper self e
    | _ ->
      default_expr_mapper self
        {
          e with
          pexp_desc =
            Pexp_match (pvb_expr, [{pc_lhs = p; pc_guard = None; pc_rhs = body}]);
          pexp_attributes = e.pexp_attributes @ pvb_attributes;
        })
  (* let [@warning "a"] {a;b} = c in body
     The attribute is attached to value binding,
     after the transformation value binding does not exist so we attach
     the attribute to the whole expression, in general, when shuffuling the ast
     it is very hard to place attributes correctly
  *)
  (* module M = await Belt.List *)
  | Pexp_letmodule
      (lid, ({pmod_desc = Pmod_ident {txt}; pmod_attributes} as me), expr)
    when Res_parsetree_viewer.has_await_attribute pmod_attributes ->
    let safe_module_type_lid : Ast_helper.lid =
      {txt = Lident (local_module_type_name txt); loc = me.pmod_loc}
    in
    {
      e with
      pexp_desc =
        Pexp_letmodule
          ( lid,
            Ast_await.create_await_module_expression
              ~module_type_lid:safe_module_type_lid me,
            self.expr self expr );
    }
  (* module M = await (Belt.List: BeltList) *)
  | Pexp_letmodule
      ( lid,
        ({
           pmod_desc =
             Pmod_constraint
               ( {pmod_desc = Pmod_ident _; pmod_attributes = attrs1},
                 {pmty_desc = Pmty_ident mtyp_lid} );
           pmod_attributes = attrs2;
         } as me),
        expr )
    when Res_parsetree_viewer.has_await_attribute attrs1
         || Res_parsetree_viewer.has_await_attribute attrs2 ->
    {
      e with
      pexp_desc =
        Pexp_letmodule
          ( lid,
            Ast_await.create_await_module_expression ~module_type_lid:mtyp_lid
              me,
            self.expr self expr );
    }
  | _ -> default_expr_mapper self e

let expr_mapper ~async_context ~in_function_def (self : mapper)
    (e : Parsetree.expression) =
  let async_saved = !async_context in
  let result = expr_mapper ~async_context ~in_function_def self e in
  async_context := async_saved;
  let check_await () =
    if !async_context = false then
      Location.raise_errorf ~loc:e.pexp_loc
        "Await on expression not in an async context"
  in
  match e.pexp_desc with
  | Pexp_letmodule (_, {pmod_desc = Pmod_ident _; pmod_attributes}, _)
    when Ast_attributes.has_await_payload pmod_attributes ->
    check_await ();
    result
  | Pexp_letmodule
      ( _,
        {
          pmod_desc =
            Pmod_constraint
              ({pmod_desc = Pmod_ident _; pmod_attributes = attrs1}, _);
          pmod_attributes = attrs2;
        },
        _ )
    when Ast_attributes.has_await_payload attrs1
         || Ast_attributes.has_await_payload attrs2 ->
    check_await ();
    result
  | _ when Ast_attributes.has_await_payload e.pexp_attributes ->
    check_await ();
    Ast_await.create_await_expression result
  | _ -> result

let typ_mapper (self : mapper) (typ : Parsetree.core_type) =
  Ast_core_type_class_type.typ_mapper self typ

let signature_item_mapper (self : mapper) (sigi : Parsetree.signature_item) :
    Parsetree.signature_item =
  match sigi.psig_desc with
  | Psig_type (rf, tdcls) -> Ast_tdcls.handle_tdcls_in_sigi self sigi rf tdcls
  | Psig_value ({pval_attributes; pval_prim} as value_desc) -> (
    let pval_attributes = self.attributes self pval_attributes in
    if Ast_attributes.rs_externals pval_attributes pval_prim then
      Ast_external.handle_external_in_sig self value_desc sigi
    else
      match Ast_attributes.has_inline_payload pval_attributes with
      | Some ((_, PStr [{pstr_desc = Pstr_eval ({pexp_desc}, _)}]) as attr) -> (
        match pexp_desc with
        | Pexp_constant (Pconst_string (s, dec)) ->
          succeed attr pval_attributes;
          {
            sigi with
            psig_desc =
              Psig_value
                {
                  value_desc with
                  pval_prim = External_ffi_types.inline_string_primitive s dec;
                  pval_attributes = [];
                };
          }
        | Pexp_constant (Pconst_integer (s, None)) ->
          succeed attr pval_attributes;
          let s = Int32.of_string s in
          {
            sigi with
            psig_desc =
              Psig_value
                {
                  value_desc with
                  pval_prim = External_ffi_types.inline_int_primitive s;
                  pval_attributes = [];
                };
          }
        | Pexp_constant (Pconst_integer (s, Some 'n')) ->
          succeed attr pval_attributes;
          {
            sigi with
            psig_desc =
              Psig_value
                {
                  value_desc with
                  pval_prim = External_ffi_types.inline_bigint_primitive s;
                  pval_attributes = [];
                };
          }
        | Pexp_constant (Pconst_float (s, None)) ->
          succeed attr pval_attributes;
          {
            sigi with
            psig_desc =
              Psig_value
                {
                  value_desc with
                  pval_prim = External_ffi_types.inline_float_primitive s;
                  pval_attributes = [];
                };
          }
        | Pexp_construct ({txt = Lident (("true" | "false") as txt)}, None) ->
          succeed attr pval_attributes;
          {
            sigi with
            psig_desc =
              Psig_value
                {
                  value_desc with
                  pval_prim =
                    External_ffi_types.inline_bool_primitive (txt = "true");
                  pval_attributes = [];
                };
          }
        | _ -> default_mapper.signature_item self sigi)
      | Some _ | None -> default_mapper.signature_item self sigi)
  | _ -> default_mapper.signature_item self sigi

let structure_item_mapper (self : mapper) (str : Parsetree.structure_item) :
    Parsetree.structure_item =
  match str.pstr_desc with
  | Pstr_type (rf, tdcls) (* [ {ptype_attributes} as tdcl ] *) ->
    Ast_tdcls.handle_tdcls_in_stru self str rf tdcls
  | Pstr_primitive prim
    when Ast_attributes.rs_externals prim.pval_attributes prim.pval_prim ->
    Ast_external.handle_external_in_stru self prim str
  | Pstr_value
      ( Nonrecursive,
        [
          {
            pvb_pat = {ppat_desc = Ppat_var pval_name} as pvb_pat;
            pvb_expr;
            pvb_attributes;
            pvb_loc;
          };
        ] ) -> (
    let pvb_expr = self.expr self pvb_expr in
    let pvb_attributes = self.attributes self pvb_attributes in
    let has_inline_property =
      Ast_attributes.has_inline_payload pvb_attributes
    in
    match (has_inline_property, pvb_expr.pexp_desc) with
    | Some attr, Pexp_constant (Pconst_string (s, dec)) ->
      succeed attr pvb_attributes;
      {
        str with
        pstr_desc =
          Pstr_primitive
            {
              pval_name;
              pval_type = Ast_literal.type_string ();
              pval_loc = pvb_loc;
              pval_attributes = [];
              pval_prim = External_ffi_types.inline_string_primitive s dec;
            };
      }
    | Some attr, Pexp_constant (Pconst_integer (s, None)) ->
      let s = Int32.of_string s in
      succeed attr pvb_attributes;
      {
        str with
        pstr_desc =
          Pstr_primitive
            {
              pval_name;
              pval_type = Ast_literal.type_int ();
              pval_loc = pvb_loc;
              pval_attributes = [];
              pval_prim = External_ffi_types.inline_int_primitive s;
            };
      }
    | Some attr, Pexp_constant (Pconst_float (s, None)) ->
      succeed attr pvb_attributes;
      {
        str with
        pstr_desc =
          Pstr_primitive
            {
              pval_name;
              pval_type = Ast_literal.type_float;
              pval_loc = pvb_loc;
              pval_attributes = [];
              pval_prim = External_ffi_types.inline_float_primitive s;
            };
      }
    | ( Some attr,
        Pexp_construct ({txt = Lident (("true" | "false") as txt)}, None) ) ->
      succeed attr pvb_attributes;
      {
        str with
        pstr_desc =
          Pstr_primitive
            {
              pval_name;
              pval_type = Ast_literal.type_bool ();
              pval_loc = pvb_loc;
              pval_attributes = [];
              pval_prim = External_ffi_types.inline_bool_primitive (txt = "true");
            };
      }
    | _ ->
      {
        str with
        pstr_desc =
          Pstr_value
            (Nonrecursive, [{pvb_pat; pvb_expr; pvb_attributes; pvb_loc}]);
      })
  | Pstr_attribute ({txt = "config"}, _) -> str
  | _ -> default_mapper.structure_item self str

let local_module_name =
  let v = ref 0 in
  fun () ->
    incr v;
    "local_" ^ string_of_int !v

let expand_reverse (stru : Ast_structure.t) (acc : Ast_structure.t) :
    Ast_structure.t =
  if stru = [] then acc
  else (
    Typemod_hide.check stru;
    let local_module_name = local_module_name () in
    let last_loc = (List.hd stru).pstr_loc in
    let stru = List.rev stru in
    let first_loc = (List.hd stru).pstr_loc in
    let loc = {first_loc with loc_end = last_loc.loc_end} in
    let open Ast_helper in
    Str.module_ ~loc
      {
        pmb_name = {txt = local_module_name; loc};
        pmb_expr =
          {
            pmod_desc = Pmod_structure stru;
            pmod_loc = loc;
            pmod_attributes = [];
          };
        pmb_attributes = Typemod_hide.attrs;
        pmb_loc = loc;
      }
    :: Str.open_ ~loc
         {
           popen_lid = {txt = Lident local_module_name; loc};
           popen_override = Override;
           popen_loc = loc;
           popen_attributes = [];
         }
    :: acc)

let rec structure_mapper ~await_context (self : mapper) (stru : Ast_structure.t)
    =
  match stru with
  | [] -> []
  | item :: rest -> (
    match item.pstr_desc with
    | Pstr_extension (({txt = "raw"; loc}, payload), _attrs) ->
      Ast_exp_handle_external.handle_raw_structure loc payload
      :: structure_mapper ~await_context self rest
    (* | Pstr_extension (({txt = "i"}, _),_)
       ->
       structure_mapper self rest *)
    | Pstr_extension (({txt = "private"}, _), _) ->
      let rec aux acc (rest : Ast_structure.t) =
        match rest with
        | {pstr_desc = Pstr_extension (({txt = "private"; loc}, payload), _)}
          :: next -> (
          match payload with
          | PStr work ->
            if List.length work = 0 then
              Location.raise_errorf ~loc
                {|%%%%private extension expects a definition as its argument. Example: %%%%private(let a = "Hello")|};
            aux
              (Ext_list.rev_map_append work acc (fun x ->
                   self.structure_item self x))
              next
          | PSig _ | PTyp _ | PPat _ ->
            Location.raise_errorf ~loc "private extension is not support")
        | _ -> expand_reverse acc (structure_mapper ~await_context self rest)
      in
      aux [] stru
    (* Dynamic import of module transformation: module M = @res.await Belt.List *)
    | Pstr_module
        ({pmb_expr = {pmod_desc = Pmod_ident {txt; loc}; pmod_attributes} as me}
         as mb)
      when Res_parsetree_viewer.has_await_attribute pmod_attributes ->
      let item = self.structure_item self item in
      let safe_module_type_name = local_module_type_name txt in
      let has_local_module_name =
        Hashtbl.find_opt !await_context safe_module_type_name
      in
      (* module __Belt_List__ = module type of Belt.List *)
      let module_type_decl =
        match has_local_module_name with
        | Some _ -> []
        | None ->
          Hashtbl.add !await_context safe_module_type_name safe_module_type_name;
          [
            Ast_helper.(
              Str.modtype ~loc
                (Mtd.mk ~loc
                   {txt = safe_module_type_name; loc}
                   ~typ:(Mty.typeof_ ~loc me)));
          ]
      in
      let safe_module_type_lid : Ast_helper.lid =
        {txt = Lident safe_module_type_name; loc = mb.pmb_expr.pmod_loc}
      in
      module_type_decl
      @
      (* module M = @res.await Belt.List *)
      {
        item with
        pstr_desc =
          Pstr_module
            {
              mb with
              pmb_expr =
                Ast_await.create_await_module_expression
                  ~module_type_lid:safe_module_type_lid mb.pmb_expr;
            };
      }
      :: structure_mapper ~await_context self rest
    | Pstr_value (_, vbs) ->
      let item = self.structure_item self item in
      (* [ module __Belt_List__ = module type of Belt.List ] *)
      let rec spelunk_vbs acc vbs =
        match vbs with
        | [] -> acc
        | ({pvb_expr} : Parsetree.value_binding) :: tl ->
          let rec aux (expr : Parsetree.expression) =
            match expr.pexp_desc with
            | Pexp_letmodule
                ( _,
                  ({pmod_desc = Pmod_ident {txt; loc}; pmod_attributes} as me),
                  expr )
              when Res_parsetree_viewer.has_await_attribute pmod_attributes -> (
              let safe_module_type_name = local_module_type_name txt in
              let has_local_module_name =
                Hashtbl.find_opt !await_context safe_module_type_name
              in

              match has_local_module_name with
              | Some _ -> aux expr
              | None ->
                Hashtbl.add !await_context safe_module_type_name
                  safe_module_type_name;
                Ast_helper.(
                  Str.modtype ~loc
                    (Mtd.mk ~loc
                       {txt = safe_module_type_name; loc}
                       ~typ:(Mty.typeof_ ~loc me)))
                :: aux expr)
            | Pexp_let (_, vbs, expr) -> aux expr @ spelunk_vbs acc vbs
            | Pexp_ifthenelse (_, then_expr, Some else_expr) ->
              aux then_expr @ aux else_expr
            | Pexp_construct (_, Some expr) -> aux expr
            | Pexp_fun {rhs = expr} | Pexp_newtype (_, expr) -> aux expr
            | _ -> acc
          in
          aux pvb_expr @ spelunk_vbs acc tl
      in
      let module_type_decls = spelunk_vbs [] vbs in

      module_type_decls @ (item :: structure_mapper ~await_context self rest)
    | _ ->
      self.structure_item self item :: structure_mapper ~await_context self rest
    )

let structure_mapper ~await_context (self : mapper) (stru : Ast_structure.t) =
  let await_saved = !await_context in
  let result =
    structure_mapper ~await_context:(ref (Hashtbl.create 10)) self stru
  in
  await_context := await_saved;
  result

let mapper : mapper =
  {
    default_mapper with
    expr = expr_mapper ~async_context:(ref true) ~in_function_def:(ref false);
    pat = pat_mapper;
    typ = typ_mapper;
    signature_item = signature_item_mapper;
    value_bindings = Ast_tuple_pattern_flatten.value_bindings_mapper;
    structure_item = structure_item_mapper;
    structure = structure_mapper ~await_context:(ref (Hashtbl.create 10));
    (* Ad-hoc way to internalize stuff *)
    label_declaration =
      (fun self lbl ->
        let lbl = default_mapper.label_declaration self lbl in
        match lbl.pld_attributes with
        | [({txt = "internal"}, _)] ->
          {
            lbl with
            pld_name =
              {lbl.pld_name with txt = String.capitalize_ascii lbl.pld_name.txt};
            pld_attributes = [];
          }
        | _ -> lbl);
  }
