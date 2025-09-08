(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

let map_row_fields_into_ints ptyp_loc (row_fields : Parsetree.row_field list) =
  let _, acc =
    Ext_list.fold_left row_fields (0, []) (fun (i, acc) rtag ->
        match rtag with
        | Rtag ({txt}, attrs, true, []) ->
          let i =
            match Ast_attributes.iter_process_bs_int_as attrs with
            | Some i -> i
            | None -> i
          in
          (i + 1, (txt, i) :: acc)
        | _ -> Bs_syntaxerr.err ptyp_loc Invalid_bs_int_type)
  in
  List.rev acc

(** Note this is okay with enums, for variants,
    the underlying representation may change due to       
    unbox
*)
let map_constructor_declarations_into_ints
    (row_fields : Parsetree.constructor_declaration list) =
  let mark = ref `nothing in
  let _, acc =
    Ext_list.fold_left row_fields (0, []) (fun (i, acc) rtag ->
        let attrs = rtag.pcd_attributes in
        match Ast_attributes.iter_process_bs_int_as attrs with
        | Some j ->
          if j <> i then if i = 0 then mark := `offset j else mark := `complex;
          (j + 1, j :: acc)
        | None -> (i + 1, i :: acc))
  in
  match !mark with
  | `nothing -> `Offset 0
  | `offset j -> `Offset j
  | `complex -> `New (List.rev acc)

(** It also check in-consistency of cases like 
    {[ [`a  | `c of int ] ]}       
*)
let map_row_fields_into_strings ptyp_loc (row_fields : Parsetree.row_field list)
    : External_arg_spec.attr =
  let has_bs_as = ref false in
  let case, result =
    Ext_list.fold_right row_fields (`Nothing, []) (fun tag (nullary, acc) ->
        match (nullary, tag) with
        | (`Nothing | `Null), Rtag ({txt}, attrs, true, []) ->
          let name =
            match Ast_attributes.iter_process_bs_string_as attrs with
            | Some name ->
              has_bs_as := true;
              name
            | None -> txt
          in
          (`Null, (txt, name) :: acc)
        | (`Nothing | `NonNull), Rtag ({txt}, attrs, false, [_]) ->
          let name =
            match Ast_attributes.iter_process_bs_string_as attrs with
            | Some name ->
              has_bs_as := true;
              name
            | None -> txt
          in
          (`NonNull, (txt, name) :: acc)
        | _ -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type)
  in
  match case with
  | `Nothing -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type
  | `Null | `NonNull -> (
    let has_payload = case = `NonNull in
    let descr = if !has_bs_as then Some result else None in
    match (has_payload, descr) with
    | false, None ->
      Location.prerr_warning ptyp_loc
        (Bs_ffi_warning "@string is redundant here, you can safely remove it");
      Nothing
    | false, Some descr -> External_arg_spec.Poly_var_string {descr}
    | true, _ -> External_arg_spec.Poly_var {descr})

let is_enum row_fields =
  List.for_all
    (fun (x : Parsetree.row_field) ->
      match x with
      | Rtag (_label, _attrs, true, []) -> true
      | _ -> false)
    row_fields

let is_enum_polyvar (ty : Parsetree.type_declaration) =
  match ty.ptype_manifest with
  | Some {ptyp_desc = Ptyp_variant (row_fields, Closed, None)}
    when is_enum row_fields ->
    Some row_fields
  | _ -> None
