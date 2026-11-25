(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(**used in effect analysis, it is sound but not-complete *)
let not_zero_constant (x : Lambda.structured_constant) =
  match x with
  | Const_int i -> i <> 0l
  | Const_assertfalse -> false
  | Const_bigint (_, i) -> i <> "0"
  | _ -> false

let rec no_side_effects (lam : Lambda.t) : bool =
  match lam with
  | Lvar _ | Lconst _ | Lfunction _ -> true
  | Lglobal_module _ -> true
  (* we record side effect in the global level,
     this expression itself is side effect free
  *)
  | Lprim {primitive; args; _} -> (
    List.for_all no_side_effects args
    &&
    match primitive with
    | Pmodint | Pdivint | Pdivbigint | Pmodbigint -> (
      match args with
      | [_; Lconst cst] -> not_zero_constant cst
      | _ -> false)
    | Ppowbigint -> (
      (* Raises on a negative exponent, so pure only when the exponent is a
         nonnegative constant. *)
      match args with
      | [_; Lconst (Const_bigint (true, _))] -> true
      | _ -> false)
    | Pcreate_extension _ | Ptypeof | Pis_null | Pis_not_none | Psome
    | Psome_not_nest | Pis_null_undefined | Pnull_to_opt
    | Pnull_undefined_to_opt | Pjs_object_create _ | Pimport _
    (* TODO: check *)
    | Pmakeblock _
    (* whether it's mutable or not *)
    | Pfield _ | Pval_from_option | Pval_from_option_not_nest
    (* NOP The compiler already [t option] is the same as t *)
    | Pduprecord | Precord_rest _
    (* generic primitives *)
    | Pobjcomp _ | Pobjorder | Pobjmin | Pobjmax | Pobjtag | Pobjsize
    (* bool primitives *)
    | Psequand | Psequor | Pnot | Pboolcomp _ | Pboolorder | Pboolmin | Pboolmax
    (* int primitives *)
    | Pnegint | Paddint | Psubint | Pmulint | Ppowint | Pnotint | Pandint
    | Porint | Pxorint | Plslint | Plsrint | Pasrint | Pintcomp _ | Pintorder
    | Pintmin | Pintmax
    (* float primitives *)
    | Pintoffloat | Pfloatofint | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat
    | Ppowfloat | Pdivfloat | Pmodfloat | Pfloatcomp _ | Pjscomp _ | Pfloatorder
    | Pfloatmin | Pfloatmax
    (* bigint primitives *)
    | Pnegbigint | Paddbigint | Psubbigint | Pmulbigint | Pnotbigint
    | Pandbigint | Porbigint | Pxorbigint | Plslbigint | Pasrbigint
    | Pbigintcomp _ | Pbigintorder | Pbigintmin | Pbigintmax
    (* string primitives *)
    | Pstringlength | Pstringcomp _ | Pstringorder | Pstringmin | Pstringmax
    (* array primitives *)
    | Pmakearray | Parraylength | Parrayrefu
    (* list primitives *)
    | Pmakelist
    (* dict primitives *)
    | Pmakedict | Pdict_has
    (* Test if the argument is a block or an immediate integer *)
    | Pis_poly_var_block
    (* Test if the (integer) argument is outside an interval *)
    (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
    (* Compile time constants *)
    | Pstringadd | Ptemplate _ | Phash | Phash_mixstring | Phash_mixint
    | Phash_finalmix
    | Praw_js_code
        {code_info = Exp (Js_function _ | Js_literal _) | Stmt Js_stmt_comment}
      ->
      true
    (* A tagged template invokes its tag at runtime, so it always has side
       effects. *)
    | Ptagged_template _ | Pjs_call _ | Pjsx _ | Pinit_mod | Pupdate_mod
    (* A spread reads the source's properties, which may run getters or
       proxy traps *)
    | Pdict_spread | Pjs_object_get _ | Pjs_object_set _ | Pdebugger
    (* Runtime calls, like the [Lapply] they were parsed into *)
    | Parray_spread | Plist_spread | Pjs_fn_method
    (* Await promise *)
    | Pawait
    (* TODO *)
    | Praw_js_code _
    (* byte swap *)
    | Parraysetu | Praise | Psetfield _
    (* bounds-checked reads throw when the index is out of range *)
    | Parrayrefs | Pstringrefs | Pstringrefu ->
      false)
  | Llet (_, _, arg, body) -> no_side_effects arg && no_side_effects body
  | Lswitch (_, _) -> false
  | Lstringswitch (_, _, _) -> false
  | Lstaticraise _ -> false
  | Lstaticcatch _ -> false
  (* It would be nice that we can also analysis some small functions
      for example [String.contains]
  *)
  | Ltrywith (body, _exn, handler) ->
    no_side_effects body && no_side_effects handler
  | Ltryfinally (body, finally) ->
    no_side_effects body && no_side_effects finally
  | Lifthenelse (a, b, c) ->
    no_side_effects a && no_side_effects b && no_side_effects c
  | Lsequence (a, b) -> no_side_effects a && no_side_effects b
  | Lbreak | Lcontinue -> false
  | Lletrec (bindings, body) ->
    Ext_list.for_all_snd bindings no_side_effects && no_side_effects body
  | Lwhile _ ->
    false (* conservative here, non-terminating loop does have side effect *)
  | Lfor _ -> false
  | Lfor_of _ | Lfor_await_of _ -> false
  | Lassign _ -> false (* actually it depends ... *)
  | Lapply _ -> false
(* we need purity analysis .. *)
