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

type ap_info = Lambda.ap_info = {
  ap_loc: Location.t;
  ap_inlined: Lambda.inline_attribute;
}

type ident = Ident.t
type lambda_switch = Lambda.lambda_switch
type apply = Lambda.lambda_apply
type lfunction = Lambda.lfunction
type prim_info = Lambda.prim_info
type t = Lambda.lambda

(* Lam and Lambda are one type. What is left here is the naming the optimizer
   uses; the constructors, their normalizations and the traversals all live in
   Lambda. *)

let var = Lambda.var
let global_module = Lambda.global_module
let const = Lambda.const
let apply = Lambda.apply
let function_ = Lambda.function_
let let_ = Lambda.let_
let letrec = Lambda.letrec
let prim = Lambda.prim
let switch = Lambda.switch
let stringswitch = Lambda.stringswitch
let staticraise = Lambda.staticraise
let staticcatch = Lambda.staticcatch
let try_ = Lambda.try_
let if_ = Lambda.if_
let seq = Lambda.seq
let break = Lambda.break
let continue = Lambda.continue
let while_ = Lambda.while_
let for_ = Lambda.for_
let for_of = Lambda.for_of
let for_await_of = Lambda.for_await_of
let assign = Lambda.assign
let not_ = Lambda.not_
let sequor = Lambda.sequor
let sequand = Lambda.sequand
let false_ = Lambda.lambda_false
let unit = Lambda.lambda_unit
let shallow_map_sharing = Lambda.shallow_map_sharing
let eq_approx = Lambda.eq_approx
