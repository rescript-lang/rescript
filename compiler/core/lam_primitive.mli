(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

type ident = Ident.t

type t =
  | Pmakeblock of Lam_tag_info.t
  | Pfield of int * Lambda.field_dbg_info
  | Psetfield of int * Lambda.set_field_dbg_info
  | Pduprecord
  | Ptagged_template
  | Precord_rest of string list
  | Pjs_call of {
      (* Location.t *  [loc] is passed down *)
      prim_name: string;
      arg_types: External_arg_spec.params;
      ffi: External_ffi_types.external_decl;
      transformed_jsx: bool;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  | Praise
  (* object primitives *)
  | Pobjcomp of Lam_compat.comparison
  | Pobjorder
  | Pobjmin
  | Pobjmax
  | Pobjtag
  | Pobjsize
  (* bool primitives *)
  | Psequand
  | Psequor
  | Pnot
  | Pboolcomp of Lam_compat.comparison
  | Pboolorder
  | Pboolmin
  | Pboolmax
  (* int primitives *)
  | Pisint
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Ppowint
  | Pandint
  | Porint
  | Pxorint
  | Pnotint
  | Plslint
  | Plsrint
  | Pasrint
  | Poffsetint of int
  | Poffsetref of int
  | Pintcomp of Lam_compat.comparison
  | Pintorder
  | Pintmin
  | Pintmax
  (* float primitives *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pmodfloat
  | Ppowfloat
  | Pfloatcomp of Lam_compat.comparison
  | Pfloatorder
  | Pfloatmin
  | Pfloatmax
  (* bigint primitives *)
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
  | Pnotbigint
  | Plslbigint
  | Pasrbigint
  | Pbigintcomp of Lam_compat.comparison
  | Pbigintorder
  | Pbigintmin
  | Pbigintmax
  (* string primitives *)
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
  | Pdict_has
  (* promise *)
  | Pawait
  (* etc or deprecated *)
  | Pis_poly_var_block
  | Pisout of int
  | Pjscomp of Lam_compat.comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pdebugger
  | Pjs_object_get of string
  | Pjs_object_set of string
  | Pinit_mod
  | Pupdate_mod
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_method
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  | Pis_null
  | Pis_undefined
  | Pis_null_undefined
  | Pimport of Lambda.import_source
  | Ptypeof
  | Pcreate_extension of string
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest
  | Phash
  | Phash_mixstring
  | Phash_mixint
  | Phash_finalmix

val is_immutable_block : Lam_tag_info.t -> bool

val eq_primitive_approx : t -> t -> bool
