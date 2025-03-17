(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript 
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

(** Warning unused bs attributes
    Note if we warn `deriving` too, 
    it may fail third party ppxes
*)
let is_bs_attribute txt =
  match txt with
  | "as" | "config" | "ignore" | "int" | "optional" | "string" | "unwrap" ->
    true
  | _ -> false

let warn_unused_attribute ((({txt; loc} as sloc), _) : Parsetree.attribute) =
  if
    is_bs_attribute txt && (not loc.loc_ghost)
    && not (Used_attributes.is_used_attribute sloc)
  then
    (*
         dump_used_attributes Format.err_formatter;
       dump_attribute Format.err_formatter attr ;
    *)
    Location.prerr_warning loc (Bs_unused_attribute txt)

let warn_discarded_unused_attributes (attrs : Parsetree.attributes) =
  if attrs <> [] then Ext_list.iter attrs warn_unused_attribute

type iterator = Ast_iterator.iterator

let super = Ast_iterator.default_iterator

let check_constant loc (const : Parsetree.constant) =
  match const with
  | Pconst_string (_, Some s) ->
    if Ast_utf8_string_interp.is_unescaped s then
      Bs_warnings.error_unescaped_delimiter loc s
  | Pconst_integer (s, None) -> (
    (* range check using int32
       It is better to give a warning instead of error to avoid make people unhappy.
       It also has restrictions in which platform bsc is running on since it will
       affect int ranges
    *)
    try ignore @@ Int32.of_string s
    with _ -> Bs_warnings.warn_literal_overflow loc)
  | _ -> ()

(* Note we only used Bs_ast_iterator here, we can reuse compiler-libs instead of
   rolling our own*)
let emit_external_warnings : iterator =
  {
    super with
    type_declaration =
      (fun self ptyp ->
        let txt = ptyp.ptype_name.txt in
        if Ast_core_type.is_builtin_rank0_type txt then
          Location.raise_errorf ~loc:ptyp.ptype_loc
            "built-in type `%s` can not be redefined " txt;
        super.type_declaration self ptyp);
    attribute = (fun _ attr -> warn_unused_attribute attr);
    structure_item =
      (fun self str_item ->
        match str_item.pstr_desc with
        | Pstr_type
            ( Nonrecursive,
              [{ptype_kind = Ptype_variant ({pcd_res = Some _} :: _)}] ) ->
          Location.raise_errorf ~loc:str_item.pstr_loc
            "GADT has to be recursive types, please try `type rec'"
        | _ -> super.structure_item self str_item);
    expr =
      (fun self ({pexp_loc = loc} as a) ->
        match a.pexp_desc with
        | Pexp_constant const -> check_constant loc const
        | Pexp_variant (s, None) when Ext_string.is_valid_hash_number s -> (
          try ignore (Ext_string.hash_number_as_i32_exn s : int32)
          with _ ->
            Location.raise_errorf ~loc
              "This number is too large to cause int overlow")
        | _ -> super.expr self a);
    label_declaration =
      (fun self lbl ->
        Ext_list.iter lbl.pld_attributes (fun attr ->
            match attr with
            | {txt = "as"}, _ -> Used_attributes.mark_used_attribute attr
            | _ -> ());
        super.label_declaration self lbl);
    constructor_declaration =
      (fun self ({pcd_name = {txt; loc}} as ctr) ->
        let _ =
          Ast_untagged_variants.process_tag_type
            ctr.pcd_attributes (* mark @as used in variant cases *)
        in
        (match txt with
        | "false" | "true" | "()" ->
          Location.raise_errorf ~loc "%s can not be redefined " txt
        | _ -> ());
        super.constructor_declaration self ctr);
    value_description =
      (fun self v ->
        match v with
        | ({pval_loc; pval_prim = [byte_name]; pval_type} :
            Parsetree.value_description) -> (
          match byte_name with
          | "%identity" when not (Ast_core_type.is_arity_one pval_type) ->
            Location.raise_errorf ~loc:pval_loc
              "%%identity expects a function type of the form 'a => 'b (arity \
               1)"
          | _ ->
            if byte_name <> "" then
              let c = String.unsafe_get byte_name 0 in
              if not (c = '%' || c = '#' || c = '?') then
                Location.prerr_warning pval_loc
                  (Warnings.Bs_ffi_warning
                     (byte_name ^ " such externals are unsafe"))
              else super.value_description self v
            else
              Location.prerr_warning pval_loc
                (Warnings.Bs_ffi_warning
                   (byte_name ^ " such externals are unsafe")))
        | _ -> super.value_description self v);
    pat =
      (fun self (pat : Parsetree.pattern) ->
        match pat.ppat_desc with
        | Ppat_constant constant -> check_constant pat.ppat_loc constant
        | _ -> super.pat self pat);
  }

let rec iter_warnings_on_stru (stru : Parsetree.structure) =
  match stru with
  | [] -> ()
  | head :: rest -> (
    match head.pstr_desc with
    | Pstr_attribute attr ->
      Builtin_attributes.warning_attribute attr;
      iter_warnings_on_stru rest
    | _ -> ())

let rec iter_warnings_on_sigi (stru : Parsetree.signature) =
  match stru with
  | [] -> ()
  | head :: rest -> (
    match head.psig_desc with
    | Psig_attribute attr ->
      Builtin_attributes.warning_attribute attr;
      iter_warnings_on_sigi rest
    | _ -> ())

let emit_external_warnings_on_structure (stru : Parsetree.structure) =
  emit_external_warnings.structure emit_external_warnings stru

let emit_external_warnings_on_signature (sigi : Parsetree.signature) =
  emit_external_warnings.signature emit_external_warnings sigi
