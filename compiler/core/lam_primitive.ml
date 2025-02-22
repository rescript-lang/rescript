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

[@@@warning "+9"]

type ident = Ident.t

type record_representation =
  | Record_regular
  | Record_inlined of {tag: int; name: string; num_nonconsts: int}
    (* Inlined record *)
  | Record_extension
(* Inlined record under extension *)

type t =
  (* Operations on heap blocks *)
  | Pmakeblock of int * Lam_tag_info.t * Asttypes.mutable_flag
  | Pfield of int * Lam_compat.field_dbg_info
  | Psetfield of int * Lam_compat.set_field_dbg_info
  (* could have field info at least for record *)
  | Pduprecord
  (* External call *)
  | Pjs_call of {
      prim_name: string;
      arg_types: External_arg_spec.params;
      ffi: External_ffi_types.external_spec;
      dynamic_import: bool;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  (* Exceptions *)
  | Praise
  (* object primitives *)
  | Pobjcomp of Lam_compat.comparison
  | Pobjorder
  | Pobjmin
  | Pobjmax
  | Pobjtag
  | Pobjsize
  (* Boolean primitives *)
  | Psequand
  | Psequor
  | Pnot
  | Pboolcomp of Lam_compat.comparison
  | Pboolorder
  | Pboolmin
  | Pboolmax
  (* Integer primitives *)
  | Pisint
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Poffsetint of int
  | Poffsetref of int
  | Pintcomp of Lam_compat.comparison
  | Pintorder
  | Pintmin
  | Pintmax
  (* Float primitives *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pmodfloat
  | Pfloatcomp of Lam_compat.comparison
  | Pfloatorder
  | Pfloatmin
  | Pfloatmax
  (* BigInt operations *)
  | Pnegbigint
  | Paddbigint
  | Psubbigint
  | Pmulbigint
  | Pdivbigint
  | Pmodbigint
  | Ppowbigint
  | Pandbigint
  | Porbigint
  | Pxorbigint
  | Plslbigint
  | Pasrbigint
  | Pbigintcomp of Lam_compat.comparison
  | Pbigintorder
  | Pbigintmin
  | Pbigintmax
  (* String primitives *)
  | Pstringlength
  | Pstringrefu
  | Pstringrefs
  | Pstringadd
  | Pstringcomp of Lam_compat.comparison
  | Pstringorder
  | Pstringmin
  | Pstringmax
  (* Array primitives *)
  | Pmakearray
  | Parraylength
  | Parrayrefu
  | Parraysetu
  | Parrayrefs
  | Parraysets
  (* List primitives *)
  | Pmakelist
  (* dict primitives *)
  | Pmakedict
  (* promise *)
  | Pawait
  (* etc or deprecated *)
  | Pis_poly_var_block
  | Pisout of int
  | Pjscomp of Lam_compat.comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pjs_runtime_apply (* [f; [...]] *)
  | Pdebugger
  | Pjs_unsafe_downgrade of {name: string; setter: bool}
  | Pinit_mod
  | Pupdate_mod
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_make of int
  | Pjs_fn_make_unit
  (* we wrap it when do the conversion to prevent
     accendential optimization
     play safe first
  *)
  | Pjs_fn_method
  | Pundefined_to_opt
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  | Pis_null
  | Pis_undefined
  | Pis_null_undefined
  | Pimport
  | Ptypeof
  | Pfn_arity
  | Pwrap_exn
    (* convert either JS exception or OCaml exception into OCaml format *)
  | Pcreate_extension of string
  | Pis_not_none (* no info about its type *)
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest
  | Phash
  | Phash_mixstring
  | Phash_mixint
  | Phash_finalmix

let eq_field_dbg_info (x : Lam_compat.field_dbg_info)
    (y : Lam_compat.field_dbg_info) =
  x = y
(* save it to avoid conditional compilation, fix it later *)

let eq_set_field_dbg_info (x : Lam_compat.set_field_dbg_info)
    (y : Lam_compat.set_field_dbg_info) =
  x = y
(* save it to avoid conditional compilation, fix it later *)

let eq_tag_info (x : Lam_tag_info.t) y = x = y

let eq_primitive_approx (lhs : t) (rhs : t) =
  match lhs with
  | Pwrap_exn | Praise
  (* generic comparison *)
  | Pobjorder | Pobjmin | Pobjmax | Pobjtag | Pobjsize
  (* bool primitives *)
  | Psequand | Psequor | Pnot | Pboolcomp _ | Pboolorder | Pboolmin | Pboolmax
  (* int primitives *)
  | Pisint | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint | Pandint
  | Porint | Pxorint | Plslint | Plsrint | Pasrint | Pintorder | Pintmin
  | Pintmax
  (* float primitives *)
  | Pintoffloat | Pfloatofint | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat
  | Pdivfloat | Pmodfloat | Pfloatorder | Pfloatmin | Pfloatmax
  (* bigint primitives *)
  | Pnegbigint | Paddbigint | Psubbigint | Pmulbigint | Pdivbigint | Pmodbigint
  | Ppowbigint | Pandbigint | Porbigint | Pxorbigint | Plslbigint | Pasrbigint
  | Pbigintorder | Pbigintmin | Pbigintmax
  (* string primitives *)
  | Pstringlength | Pstringrefu | Pstringrefs | Pstringadd | Pstringcomp _
  | Pstringorder | Pstringmin | Pstringmax
  (* List primitives *)
  | Pmakelist
  (* dict primitives *)
  | Pmakedict
  (* promise *)
  | Pawait
  (* etc *)
  | Pjs_apply | Pjs_runtime_apply | Pval_from_option | Pval_from_option_not_nest
  | Pundefined_to_opt | Pnull_to_opt | Pnull_undefined_to_opt | Pis_null
  | Pis_not_none | Psome | Psome_not_nest | Pis_undefined | Pis_null_undefined
  | Pimport | Ptypeof | Pfn_arity | Pis_poly_var_block | Pdebugger | Pinit_mod
  | Pupdate_mod | Pduprecord | Pmakearray | Parraylength | Parrayrefu
  | Parraysetu | Parrayrefs | Parraysets | Pjs_fn_make_unit | Pjs_fn_method
  | Phash | Phash_mixstring | Phash_mixint | Phash_finalmix ->
    rhs = lhs
  | Pcreate_extension a -> (
    match rhs with
    | Pcreate_extension b -> a = (b : string)
    | _ -> false)
  | Pisout l -> (
    match rhs with
    | Pisout r -> l = r
    | _ -> false)
  (* | Pcaml_obj_set_length -> rhs = Pcaml_obj_set_length *)
  | Pfield (n0, info0) -> (
    match rhs with
    | Pfield (n1, info1) -> n0 = n1 && eq_field_dbg_info info0 info1
    | _ -> false)
  | Psetfield (i0, info0) -> (
    match rhs with
    | Psetfield (i1, info1) -> i0 = i1 && eq_set_field_dbg_info info0 info1
    | _ -> false)
  | Pmakeblock (i0, info0, flag0) -> (
    match rhs with
    | Pmakeblock (i1, info1, flag1) ->
      i0 = i1 && flag0 = flag1 && eq_tag_info info0 info1
    | _ -> false)
  | Pjs_call {prim_name; arg_types; ffi; dynamic_import} -> (
    match rhs with
    | Pjs_call rhs ->
      prim_name = rhs.prim_name && arg_types = rhs.arg_types && ffi = rhs.ffi
      && dynamic_import = rhs.dynamic_import
    | _ -> false)
  | Pjs_object_create obj_create -> (
    match rhs with
    | Pjs_object_create obj_create1 -> obj_create = obj_create1
    | _ -> false)
  | Pobjcomp comparison -> (
    match rhs with
    | Pobjcomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
    | _ -> false)
  | Pintcomp comparison -> (
    match rhs with
    | Pintcomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
    | _ -> false)
  | Pfloatcomp comparison -> (
    match rhs with
    | Pfloatcomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
    | _ -> false)
  | Pbigintcomp comparison -> (
    match rhs with
    | Pbigintcomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
    | _ -> false)
  | Pjscomp comparison -> (
    match rhs with
    | Pjscomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
    | _ -> false)
  | Poffsetint i0 -> (
    match rhs with
    | Poffsetint i1 -> i0 = i1
    | _ -> false)
  | Poffsetref i0 -> (
    match rhs with
    | Poffsetref i1 -> i0 = i1
    | _ -> false)
  | Pjs_unsafe_downgrade {name; setter} -> (
    match rhs with
    | Pjs_unsafe_downgrade rhs -> name = rhs.name && setter = rhs.setter
    | _ -> false)
  | Pjs_fn_make i -> (
    match rhs with
    | Pjs_fn_make i1 -> i = i1
    | _ -> false)
  | Praw_js_code _ -> false
(* TOO lazy, here comparison is only approximation*)
