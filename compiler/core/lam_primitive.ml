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

type t = Lambda.primitive =
  | Pdebugger
  | Ptypeof
  | Psome
  | Psome_not_nest
      (** [Some x] where [x] cannot itself be [undefined], so no wrapping is
          needed. *)
  (* Operations on heap blocks *)
  | Pmakeblock of Lam_tag_info.t
  | Pfield of int * Lambda.field_dbg_info
  | Psetfield of int * Lambda.set_field_dbg_info
  | Pduprecord
  | Precord_rest of string list (* excluded runtime field names *)
  (* JS FFI calls, expanded from the external's spec at translation *)
  | Pjs_call of {
      prim_name: string;
      arg_types: External_arg_spec.params;
      ffi: External_ffi_types.external_decl;
      transformed_jsx: bool;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  | Pjs_object_get of string
  | Pjs_object_set of string
  (* Exceptions *)
  | Praise
  (* object primitives *)
  | Pobjcomp of Lam_compat.comparison
  | Pobjorder
  | Pobjmin
  | Pobjmax
  | Pobjtag
  | Pobjsize
  (* Boolean operations *)
  | Psequand
  | Psequor
  | Pnot
  | Pboolcomp of Lam_compat.comparison
  | Pboolorder
  | Pboolmin
  | Pboolmax
  (* Integer operations *)
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
  | Pintcomp of Lam_compat.comparison
  | Pintorder
  | Pintmin
  | Pintmax
  (* Float operations *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pmodfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Ppowfloat
  | Pfloatcomp of Lam_compat.comparison
  | Pfloatorder
  | Pfloatmin
  | Pfloatmax
  (* BigInt operations *)
  | Pnegbigint
  | Paddbigint
  | Psubbigint
  | Ppowbigint
  | Pmulbigint
  | Pdivbigint
  | Pmodbigint
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
  (* String operations *)
  | Pstringlength
  | Pstringrefu
  | Pstringrefs
  | Pstringcomp of Lam_compat.comparison
  | Pstringorder
  | Pstringmin
  | Pstringmax
  | Pstringadd
  (* Array operations *)
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
  (* modules *)
  | Pimport of Lambda.import_source
  | Pinit_mod
  | Pupdate_mod
  (* hash *)
  | Phash
  | Phash_mixint
  | Phash_mixstring
  | Phash_finalmix
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  (* Test if the argument is null or undefined *)
  | Pis_null_undefined
  (* exn *)
  | Pcreate_extension of string
  (* js *)
  | Pjscomp of Lam_compat.comparison
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  (* Produced by Lam_pass_remove_alias, not by translation *)
  | Pis_null
  | Pis_undefined
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Pis_poly_var_block
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_method
  | Ptagged_template

(* The mutability of a block is a property of its shape, so it is derived
   rather than stored alongside it. *)
let is_immutable_block (info : Lam_tag_info.t) =
  Lambda.mutable_flag_of_tag_info info = Immutable

let eq_primitive_approx = Lambda.eq_primitive_approx
