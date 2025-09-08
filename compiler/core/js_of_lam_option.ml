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

module E = Js_exp_make

type option_unwrap_time = Static_unwrapped | Runtime_maybe_unwrapped

(** Another way: 
    {[
      | Var _  ->
        can only bd detected at runtime thing
          (E.triple_equal (E.typeof arg)
             (E.str "number"))
    ]}
*)
let none : J.expression = E.undefined

let is_none_static (arg : J.expression_desc) =
  match arg with
  | Undefined _ -> true
  | _ -> false

let is_not_none (e : J.expression) : J.expression =
  let desc = e.expression_desc in
  if is_none_static desc then E.false_
  else
    match desc with
    | Optional_block _ -> E.true_
    | _ -> E.not (E.triple_equal e none)

(**
   Invrariant: 
   - optional encoding
   -  None encoding

   when no argumet is supplied, [undefined] 
   if we detect that all rest arguments are [null], 
   we can remove them


   - avoid duplicate evlauation of [arg] when it
     is not a variable
     {!Js_ast_util.named_expression} does not help 
     since we need an expression here, it might be a statement
*)
let val_from_option (arg : J.expression) =
  match arg.expression_desc with
  | Optional_block (x, _) -> x
  | _ -> E.runtime_call Primitive_modules.option "valFromOption" [arg]

let get_default_undefined_from_optional (arg : J.expression) : J.expression =
  let desc = arg.expression_desc in
  if is_none_static desc then E.undefined
  else
    match desc with
    | Optional_block (x, _) -> x (* invariant: option encoding *)
    | _ ->
      if Js_analyzer.is_okay_to_duplicate arg then
        (* FIXME: no need do such inlining*)
        E.econd (is_not_none arg) (val_from_option arg) E.undefined
      else E.runtime_call Primitive_modules.option "toUndefined" [arg]

let option_unwrap (arg : J.expression) : J.expression =
  let desc = arg.expression_desc in
  if is_none_static desc then E.undefined
  else
    match desc with
    | Optional_block (x, _) -> E.poly_var_value_access x
    (* invariant: option encoding *)
    | _ -> E.runtime_call Primitive_modules.option "unwrapPolyVar" [arg]

let destruct_optional ~for_sure_none ~for_sure_some ~not_sure
    (arg : J.expression) =
  let desc = arg.expression_desc in
  if is_none_static desc then for_sure_none
  else
    match desc with
    | Optional_block (x, _) -> for_sure_some x
    | _ -> not_sure ()

let some = E.optional_block

let null_to_opt e = E.econd (E.is_null e) none (some e)

let undef_to_opt e = E.econd (E.is_undef e) none (some e)

let null_undef_to_opt e = E.econd (E.is_null_undefined e) none (some e)
