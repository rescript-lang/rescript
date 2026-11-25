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

let no_side_effects = Lambda_effects.no_side_effects

(* The Lambda counterpart of [Lam_compile.delay_safe]: [lam] has no side
   effects and reads nothing that other code can change, so evaluating it
   later gives the same value. Fields of records, which may be mutable,
   arrays, dictionaries and the variables in [assignable] can change. *)
let rec delay_safe ~assignable (lam : Lambda.t) : bool =
  match lam with
  | Lconst _ | Lfunction _ | Lglobal_module _ -> true
  | Lvar id -> not (Set_ident.mem id assignable)
  | Lifthenelse (a, b, c) ->
    delay_safe ~assignable a && delay_safe ~assignable b
    && delay_safe ~assignable c
  | Lprim {primitive; args} -> (
    List.for_all (delay_safe ~assignable) args
    &&
    match primitive with
    | Pdivint | Pmodint | Pdivbigint | Pmodbigint | Ppowbigint ->
      (* they raise unless an operand is a suitable constant *)
      no_side_effects lam
    | Pfield
        ( _,
          ( Fld_module _ | Fld_tuple | Fld_poly_var_tag | Fld_poly_var_content
          | Fld_extension | Fld_variant | Fld_cons ) )
    (* values and fresh blocks *)
    | Ptypeof | Psome | Psome_not_nest | Pmakeblock _ | Pjs_object_create _
    | Pmakearray | Pmakelist | Pmakedict | Ptemplate _ | Pstringadd
    | Pstringlength | Pstringcomp _ | Pstringorder | Pstringmin | Pstringmax
    | Psequand | Psequor | Pnot | Pboolcomp _ | Pboolorder | Pboolmin | Pboolmax
    | Pnegint | Paddint | Psubint | Pmulint | Ppowint | Pandint | Porint
    | Pxorint | Pnotint | Plslint | Plsrint | Pasrint | Pintcomp _ | Pintorder
    | Pintmin | Pintmax | Pintoffloat | Pfloatofint | Pnegfloat | Pmodfloat
    | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat | Ppowfloat | Pfloatcomp _
    | Pfloatorder | Pfloatmin | Pfloatmax | Pnegbigint | Paddbigint | Psubbigint
    | Pmulbigint | Pandbigint | Porbigint | Pxorbigint | Pnotbigint | Plslbigint
    | Pasrbigint | Pbigintcomp _ | Pbigintorder | Pbigintmin | Pbigintmax
    | Phash_mixint | Phash_mixstring | Phash_finalmix | Pis_null_undefined
    | Pjscomp _ | Pnull_to_opt | Pnull_undefined_to_opt | Pis_null
    | Pis_not_none | Pval_from_option | Pval_from_option_not_nest
    | Pis_poly_var_block ->
      true
    (* reads of the heap, and primitives with side effects *)
    | Pfield (_, (Fld_record _ | Fld_record_inline _ | Fld_record_extension _))
    | Pdebugger | Psetfield _ | Pduprecord | Precord_rest _ | Pjs_call _
    | Pjsx _ | Pjs_object_get _ | Pjs_object_set _ | Praise | Pobjcomp _
    | Pobjorder | Pobjmin | Pobjmax | Pobjtag | Pobjsize | Pstringrefu
    | Pstringrefs | Parraylength | Parrayrefu | Parraysetu | Parrayrefs
    | Pdict_has | Pawait | Pimport _ | Pinit_mod | Pupdate_mod | Phash
    | Pcreate_extension _ | Praw_js_code _ | Pjs_fn_method | Ptagged_template _
    | Pdict_spread | Parray_spread | Plist_spread ->
      false)
  | Llet _ | Lletrec _ | Lapply _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
  | Lstaticcatch _ | Ltrywith _ | Ltryfinally _ | Lsequence _ | Lwhile _
  | Lfor _ | Lfor_of _ | Lfor_await_of _ | Lassign _ | Lbreak | Lcontinue ->
    false

let reads_callee (primitive : Lambda.primitive) =
  match primitive with
  | Pjs_call {ffi = {kind = Decl_val _ | Decl_new _}} -> true
  | _ -> false

type motion = Anywhere | Reads | Effects

let motion ~assignable lam : motion =
  if delay_safe ~assignable lam then Anywhere
  else if no_side_effects lam then Reads
  else Effects

let conflict (a : motion) (b : motion) =
  match (a, b) with
  | Anywhere, _ | _, Anywhere | Reads, Reads -> false
  | Effects, (Reads | Effects) | Reads, Effects -> true

(* Size of a lambda that is too big to inline, e.g. one containing a loop *)
let too_big = 1000

exception Limit_reached

(*
   Estimate the size of a lambda for inlining. Callers only compare it with
   small thresholds, so stop counting once [limit] is reached: the result is
   the size if it is below [limit], and [limit] otherwise.
*)
let size_upto ~limit (lam : Lambda.t) =
  assert (limit <= too_big);
  let total = ref 0 in
  let add n =
    total := !total + n;
    if !total >= limit then raise_notrace Limit_reached
  in
  (* Constants count their leaves, and stop at the limit too: a large constant
     list or record should not be walked completely. *)
  let rec constant (c : Lambda.structured_constant) =
    match c with
    | Const_int _ | Const_assertfalse | Const_constructor _ | Const_char _
    | Const_float _ | Const_bigint _ | Const_polyvar _ | Const_js_null
    | Const_js_undefined _ | Const_module_alias | Const_js_true | Const_js_false
    | Const_string _ ->
      add 1
    | Const_some s -> constant s
    | Const_block (_, str) -> List.iter constant str
  in
  let rec size (lam : Lambda.t) =
    match lam with
    | Lvar _ -> add 1
    | Lconst c -> constant c
    | Llet (_, _, l1, l2) ->
      add 1;
      size l1;
      size l2
    | Lletrec _ -> add too_big
    | Lprim
        {
          primitive = Pfield (_, Fld_module _);
          args = [(Lglobal_module _ | Lvar _)];
          _;
        } ->
      add 1
    | Lprim {primitive = Praise | Pis_not_none; args = [l]; _} -> size l
    | Lglobal_module _ -> add 1
    | Lprim {primitive = Praw_js_code _} -> add too_big
    | Lprim {args = ll; _} ->
      add 1;
      List.iter size ll
    (* complicated
           1. inline this function
           2. ...
           exports.Make=
           function(funarg)
       {var $$let=Make(funarg);
         return [0, $$let[5],... $$let[16]]}
    *)
    | Lapply {ap_func; ap_args; _} ->
      size ap_func;
      List.iter size ap_args
    | Lfunction {body} -> size body
    | Lswitch _ -> add too_big
    | Lstringswitch (_, _, _) -> add too_big
    | Lstaticraise (_i, ls) ->
      add 1;
      List.iter size ls
    | Lstaticcatch _ -> add too_big
    | Ltrywith _ | Ltryfinally _ -> add too_big
    | Lifthenelse (l1, l2, l3) ->
      add 1;
      size l1;
      size l2;
      size l3
    | Lsequence (l1, l2) ->
      size l1;
      size l2
    | Lbreak | Lcontinue -> add 1
    | Lwhile _ -> add too_big
    | Lfor _ -> add too_big
    | Lfor_of _ | Lfor_await_of _ -> add too_big
    | Lassign (_, v) ->
      add 1;
      size v
    (* This is side effectful,  be careful *)
  in
  match size lam with
  | () -> !total
  | exception Limit_reached -> limit

let args_all_const (args : Lambda.t list) =
  List.for_all
    (fun (x : Lambda.t) ->
      match x with
      | Lconst _ -> true
      | _ -> false)
    args

let exit_inline_size = 7

let small_inline_size = 5

(** destruct pattern will work better 
    if it is closed lambda, otherwise
    you can not do full evaluation

    We still should avoid inline too big code, 

    ideally we should also evaluate its size after inlining, 
    since after partial evaluation, it might still be *very big*
*)
let destruct_pattern (body : Lambda.t) params args =
  let rec aux v params args =
    match (params, args) with
    | x :: xs, b :: bs -> if Ident.same x v then Some b else aux v xs bs
    | [], _ -> None
    | _ :: _, [] -> assert false
  in
  match body with
  | Lswitch (Lvar v, switch) -> (
    match aux v params args with
    | Some (Lambda.Lconst _ as lam) ->
      size_upto ~limit:small_inline_size (Lambda.switch lam switch)
      < small_inline_size
    | Some _ | None -> false)
  | Lifthenelse (Lvar v, then_, else_) -> (
    (* -FIXME *)
    match aux v params args with
    | Some (Lconst _ as lam) ->
      size_upto ~limit:small_inline_size (Lambda.if_ lam then_ else_)
      < small_inline_size
    | Some _ | None -> false)
  | _ -> false

(** Hints to inlining *)
let ok_to_inline_fun_when_app (m : Lambda.lfunction) (args : Lambda.t list) =
  match m.attr.inline with
  | Always_inline -> true
  | Never_inline -> false
  | Default_inline -> (
    match m with
    | {body; params} ->
      let s = size_upto ~limit:10 body in
      s < small_inline_size
      || destruct_pattern body params args
      || (args_all_const args && s < 10 && no_side_effects body))

(* TODO:  We can relax this a bit later,
    but decide whether to inline it later in the call site
*)
let safe_to_inline (lam : Lambda.t) =
  match lam with
  | Lfunction _ -> true
  | Lconst
      ( Const_polyvar _ | Const_constructor _ | Const_js_true | Const_js_false
      | Const_js_undefined _ ) ->
    true
  | _ -> false
