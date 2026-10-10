(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree

let out_ident = ref pp_print_string

let print_lident ppf = function
  | "::" -> !out_ident ppf "(::)"
  | s -> !out_ident ppf s

let rec print_ident ppf = function
  | Oide_ident s -> print_lident ppf s
  | Oide_dot (id, s) ->
    print_ident ppf id;
    pp_print_char ppf '.';
    print_lident ppf s
  | Oide_apply (id1, id2) ->
    fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let parenthesized_ident name =
  List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"]
  ||
  match name.[0] with
  | 'a' .. 'z' | 'A' .. 'Z' | '\223' .. '\246' | '\248' .. '\255' | '_' -> false
  | _ -> true

let value_ident ppf name =
  if parenthesized_ident name then fprintf ppf "( %s )" name
  else pp_print_string ppf name

(* Types *)

let rec print_list_init pr sep ppf = function
  | [] -> ()
  | a :: l ->
    sep ppf;
    pr ppf a;
    print_list_init pr sep ppf l

let rec print_list pr sep ppf = function
  | [] -> ()
  | [a] -> pr ppf a
  | a :: l ->
    pr ppf a;
    sep ppf;
    print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf = function
  | Otyp_alias (ty, s) -> fprintf ppf "@[%a@ as '%s@]" print_out_type ty s
  | Otyp_poly (sl, ty) ->
    fprintf ppf "@[<hov 2>%a.@ %a@]" pr_vars sl print_out_type ty
  | ty -> print_out_type_1 ppf ty

and print_out_type_1 ppf = function
  | Otyp_arrow (args, ret) ->
    pp_open_box ppf 0;
    List.iter
      (fun (lab, ty1) ->
        (match lab with
        | Asttypes.Noloc.Nolabel -> ()
        | Asttypes.Noloc.Labelled label -> fprintf ppf "%s:" label
        | Asttypes.Noloc.Optional label -> fprintf ppf "?%s:" label);
        print_out_type_2 ppf ty1;
        pp_print_string ppf " ->";
        pp_print_space ppf ())
      args;
    print_out_type_1 ppf ret;
    pp_close_box ppf ()
  | ty -> print_out_type_2 ppf ty

and print_out_type_2 ppf = function
  | Otyp_tuple tyl ->
    fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty -> print_simple_out_type ppf ty

and print_simple_out_type ppf = function
  | Otyp_constr (id, tyl) ->
    pp_open_box ppf 0;
    print_typargs ppf tyl;
    print_ident ppf id;
    pp_close_box ppf ()
  | Otyp_object (fields, rest) ->
    fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
    let print_present ppf = function
      | None | Some [] -> ()
      | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
    in
    let print_fields ppf = function
      | Ovar_fields fields ->
        print_list print_row_field
          (fun ppf -> fprintf ppf "@;<1 -2>| ")
          ppf fields
      | Ovar_typ typ -> print_simple_out_type ppf typ
    in
    fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]"
      (if non_gen then "_" else "")
      (if closed then if tags = None then " " else "< "
       else if tags = None then "> "
       else "? ")
      print_fields row_fields print_present tags
  | (Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _) as ty ->
    pp_open_box ppf 1;
    pp_print_char ppf '(';
    print_out_type ppf ty;
    pp_print_char ppf ')';
    pp_close_box ppf ()
  | Otyp_abstract | Otyp_open | Otyp_sum _ | Otyp_manifest (_, _) -> ()
  | Otyp_record lbls -> print_record_decl ppf lbls
  | Otyp_module (p, n, tyl) ->
    fprintf ppf "@[<1>(module %s" p;
    let first = ref true in
    List.iter2
      (fun s t ->
        let sep =
          if !first then (
            first := false;
            "with")
          else "and"
        in
        fprintf ppf " %s type %s = %a" sep s print_out_type t)
      n tyl;
    fprintf ppf ")@]"
  | Otyp_attribute (t, attr) ->
    fprintf ppf "@[<1>(%a [@@%s])@]" print_out_type t attr.oattr_name

and print_record_decl ppf lbls =
  fprintf ppf "{%a@;<1 -2>}"
    (print_list_init print_out_label (fun ppf -> fprintf ppf "@ "))
    lbls

and print_fields rest ppf = function
  | [] -> (
    match rest with
    | Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
    | None -> ())
  | [(s, mut, t)] ->
    fprintf ppf "%s%s : %a" (if mut then "mutable " else "") s print_out_type t;
    (match rest with
    | Some _ -> fprintf ppf ";@ "
    | None -> ());
    print_fields rest ppf []
  | (s, mut, t) :: l ->
    fprintf ppf "%s%s : %a;@ %a"
      (if mut then "mutable " else "")
      s print_out_type t (print_fields rest) l

and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of
    (print_typlist print_out_type " &")
    tyl

and print_typlist print_elem sep ppf = function
  | [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
    print_elem ppf ty;
    pp_print_string ppf sep;
    pp_print_space ppf ();
    print_typlist print_elem sep ppf tyl

and print_typargs ppf = function
  | [] -> ()
  | [ty1] ->
    print_simple_out_type ppf ty1;
    pp_print_space ppf ()
  | tyl ->
    pp_open_box ppf 1;
    pp_print_char ppf '(';
    print_typlist print_out_type "," ppf tyl;
    pp_print_char ppf ')';
    pp_close_box ppf ();
    pp_print_space ppf ()

and print_out_label ppf (name, mut, opt, arg) =
  fprintf ppf "@[<2>%s%s%s :@ %a@];"
    (if opt then "@optional " else "")
    (if mut then "mutable " else "")
    name print_out_type arg

let out_type = ref print_out_type

(* Class types *)

let type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s%s"
    (if not cn then "+" else if not co then "-" else "")
    (if ty = "_" then ty else "'" ^ ty)

(* Signature *)

let out_module_type = ref (fun _ -> failwith "Oprint.out_module_type")
let out_sig_item = ref (fun _ -> failwith "Oprint.out_sig_item")
let out_signature = ref (fun _ -> failwith "Oprint.out_signature")
let out_type_extension = ref (fun _ -> failwith "Oprint.out_type_extension")

let rec print_out_functor funct ppf = function
  | Omty_functor (_, None, mty_res) ->
    if funct then fprintf ppf "() %a" (print_out_functor true) mty_res
    else fprintf ppf "functor@ () %a" (print_out_functor true) mty_res
  | Omty_functor (name, Some mty_arg, mty_res) -> (
    match (name, funct) with
    | "_", true ->
      fprintf ppf "->@ %a ->@ %a" print_out_module_type mty_arg
        (print_out_functor false) mty_res
    | "_", false ->
      fprintf ppf "%a ->@ %a" print_out_module_type mty_arg
        (print_out_functor false) mty_res
    | name, true ->
      fprintf ppf "(%s : %a) %a" name print_out_module_type mty_arg
        (print_out_functor true) mty_res
    | name, false ->
      fprintf ppf "functor@ (%s : %a) %a" name print_out_module_type mty_arg
        (print_out_functor true) mty_res)
  | m ->
    if funct then fprintf ppf "->@ %a" print_out_module_type m
    else print_out_module_type ppf m

and print_out_module_type ppf = function
  | Omty_abstract -> ()
  | Omty_functor _ as t -> fprintf ppf "@[<2>%a@]" (print_out_functor false) t
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
    fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" !out_signature sg
  | Omty_alias id -> fprintf ppf "(module %a)" print_ident id

and print_out_signature ppf = function
  | [] -> ()
  | [item] -> !out_sig_item ppf item
  | Osig_typext (ext, Oext_first) :: items ->
    (* Gather together the extension constructors *)
    let rec gather_extensions acc items =
      match items with
      | Osig_typext (ext, Oext_next) :: items ->
        gather_extensions
          ((ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)
          :: acc)
          items
      | _ -> (List.rev acc, items)
    in
    let exts, items =
      gather_extensions
        [(ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)]
        items
    in
    let te =
      {
        otyext_name = ext.oext_type_name;
        otyext_params = ext.oext_type_params;
        otyext_constructors = exts;
        otyext_private = ext.oext_private;
      }
    in
    fprintf ppf "%a@ %a" !out_type_extension te print_out_signature items
  | item :: items ->
    fprintf ppf "%a@ %a" !out_sig_item item print_out_signature items

and print_out_sig_item ppf = function
  | Osig_typext (ext, Oext_exception) ->
    fprintf ppf "@[<2>exception %a@]" print_out_constr
      (ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)
  | Osig_typext (ext, _es) -> print_out_extension_constructor ppf ext
  | Osig_modtype (name, Omty_abstract) ->
    fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
    fprintf ppf "@[<2>module type %s =@ %a@]" name !out_module_type mty
  | Osig_module (name, Omty_alias id, _) ->
    fprintf ppf "@[<2>module %s =@ %a@]" name print_ident id
  | Osig_module (name, mty, rs) ->
    fprintf ppf "@[<2>%s %s :@ %a@]"
      (match rs with
      | Orec_not -> "module"
      | Orec_first -> "module rec"
      | Orec_next -> "and")
      name !out_module_type mty
  | Osig_type (td, rs) ->
    print_out_type_decl
      (match rs with
      | Orec_not -> "type nonrec"
      | Orec_first -> "type"
      | Orec_next -> "and")
      ppf td
  | Osig_value vd ->
    let kwd = if vd.oval_prim = None then "val" else "external" in
    let pr_prim ppf (repr : Parsetree.primitive_repr option) =
      (* this OCaml-syntax debug printer shows the external's name only; the
         ReScript outcome printer renders the full attribute syntax *)
      match repr with
      | None -> ()
      | Some (Prim_name s) | Some (Prim_ffi {name = s}) ->
        fprintf ppf "@ = \"%s\"" s
      | Some (Prim_inline_const _) -> fprintf ppf "@ = \"#rescript-inline\""
    in
    fprintf ppf "@[<2>%s %a :@ %a%a%a@]" kwd value_ident vd.oval_name !out_type
      vd.oval_type pr_prim vd.oval_prim
      (fun ppf -> List.iter (fun a -> fprintf ppf "@ [@@@@%s]" a.oattr_name))
      vd.oval_attributes
  | Osig_ellipsis -> fprintf ppf "..."

and print_out_type_decl kwd ppf td =
  let print_constraints ppf =
    List.iter
      (fun (ty1, ty2) ->
        fprintf ppf "@ @[<2>constraint %a =@ %a@]" !out_type ty1 !out_type ty2)
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
    | [] -> pp_print_string ppf td.otype_name
    | [param] -> fprintf ppf "@[%a@ %s@]" type_parameter param td.otype_name
    | _ ->
      fprintf ppf "@[(@[%a)@]@ %s@]"
        (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
        td.otype_params td.otype_name
  in
  let print_manifest ppf = function
    | Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" !out_type ty
    | _ -> ()
  in
  let print_name_params ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest td.otype_type
  in
  let ty =
    match td.otype_type with
    | Otyp_manifest (_, ty) -> ty
    | _ -> td.otype_type
  in
  let print_private ppf = function
    | Asttypes.Private -> fprintf ppf " private"
    | Asttypes.Public -> ()
  in
  let print_immediate ppf =
    if td.otype_immediate then fprintf ppf " [%@%@immediate]" else ()
  in
  let print_unboxed ppf =
    if td.otype_unboxed then fprintf ppf " [%@%@unboxed]" else ()
  in
  let print_out_tkind ppf = function
    | Otyp_abstract -> ()
    | Otyp_record lbls ->
      fprintf ppf " =%a %a" print_private td.otype_private print_record_decl
        lbls
    | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a" print_private td.otype_private
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | "))
        constrs
    | Otyp_open -> fprintf ppf " =%a .." print_private td.otype_private
    | ty ->
      fprintf ppf " =%a@;<1 2>%a" print_private td.otype_private !out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]%t%t%t@]" print_name_params print_out_tkind ty
    print_constraints print_immediate print_unboxed

and print_out_constr ppf (name, tyl, ret_type_opt, repr) =
  let () =
    match repr with
    | None -> ()
    | Some s -> pp_print_string ppf s
  in
  let name =
    match name with
    | "::" -> "(::)" (* #7200 *)
    | s -> s
  in
  match ret_type_opt with
  | None -> (
    match tyl with
    | [] -> pp_print_string ppf name
    | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (print_typlist print_simple_out_type " *")
        tyl)
  | Some ret_type -> (
    match tyl with
    | [] -> fprintf ppf "@[<2>%s :@ %a@]" name print_simple_out_type ret_type
    | _ ->
      fprintf ppf "@[<2>%s :@ %a -> %a@]" name
        (print_typlist print_simple_out_type " *")
        tyl print_simple_out_type ret_type)

and print_out_extension_constructor ppf ext =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s" (if ty = "_" then ty else "'" ^ ty)
    in
    match ext.oext_type_params with
    | [] -> fprintf ppf "%s" ext.oext_type_name
    | [ty_param] ->
      fprintf ppf "@[%a@ %s@]" print_type_parameter ty_param ext.oext_type_name
    | _ ->
      fprintf ppf "@[(@[%a)@]@ %s@]"
        (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
        ext.oext_type_params ext.oext_type_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]" print_extended_type
    (if ext.oext_private = Asttypes.Private then " private" else "")
    print_out_constr
    (ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)

and print_out_type_extension ppf te =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s" (if ty = "_" then ty else "'" ^ ty)
    in
    match te.otyext_params with
    | [] -> fprintf ppf "%s" te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %s@]" print_type_parameter param te.otyext_name
    | _ ->
      fprintf ppf "@[(@[%a)@]@ %s@]"
        (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
        te.otyext_params te.otyext_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]" print_extended_type
    (if te.otyext_private = Asttypes.Private then " private" else "")
    (print_list print_out_constr (fun ppf -> fprintf ppf "@ | "))
    te.otyext_constructors

let _ = out_module_type := print_out_module_type
let _ = out_signature := print_out_signature
let _ = out_sig_item := print_out_sig_item
let _ = out_type_extension := print_out_type_extension
