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

(** Define some basic types used in JS IR *)

type binop =
  | Eq
    (* acutally assignment ..
       TODO: move it into statement, so that all expressions
       are side efffect free (except function calls)
    *)
  | Or
  | And
  | EqEqEq
  | NotEqEq (* | InstanceOf *)
  | Lt
  | Le
  | Gt
  | Ge
  | Bor
  | Bxor
  | Band
  | Lsl
  | Lsr
  | Asr
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Pow
  | InstanceOf

(**
   note that we don't need raise [Div_by_zero] in ReScript

   {[
     let add x y = x + y  (* | 0 *)
     let minus x y = x - y (* | 0 *)
     let mul x y = x * y   (* caml_mul | Math.imul *)
     let div x y = x / y (* caml_div (x/y|0)*)
     let imod x y = x mod y  (* caml_mod (x%y) (zero_divide)*)

     let bor x y = x lor y   (* x  | y *)
     let bxor x y = x lxor y (* x ^ y *)
     let band x y = x land y (* x & y *)
     let ilnot  y  = lnot y (* let lnot x = x lxor (-1) *)
     let ilsl x y = x lsl y (* x << y*)
     let ilsr x y = x lsr y  (* x >>> y | 0 *)
     let iasr  x y = x asr y (* x >> y *)
   ]}


   Note that js treat unsigned shift 0 bits in a special way
   Unsigned shifts convert their left-hand side to Uint32, 
   signed shifts convert it to Int32.
   Shifting by 0 digits returns the converted value.
   {[
     function ToUint32(x) {
         return x >>> 0;
       }
         function ToInt32(x) {
             return x >> 0;
           }
   ]}
   So in Js, [-1 >>>0] will be the largest Uint32, while [-1>>0] will remain [-1]
   and [-1 >>> 0 >> 0 ] will be [-1]
*)
type int_op =
  | Bor
  | Bxor
  | Band
  | Lsl
  | Lsr
  | Asr
  | Plus
  (* for [+], given two numbers
     x + y | 0
  *)
  | Minus
  (* x - y | 0 *)
  | Mul
  (* *)
  | Div
  (* x / y | 0 *)
  | Mod
  (* x  % y *)
  | Pow (* x ** y | 0 *)

(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Bitwise_operators
   {[
     ~
   ]}
    ~0xff -> -256
    design; make sure each operation type is consistent
*)
type level = Log | Info | Warn | Error

type kind =
  | Ml
  | Runtime
  | External of {
      name: string;
      default: bool;
      import_attributes: External_ffi_types.import_attributes option;
    }

type property = Lam_compat.let_kind = Strict | Alias | StrictOpt | Variable

type property_name = Lit of string | Symbol_name

type 'a access = Getter | Setter

(* literal char *)
type float_lit = {f: string} [@@unboxed]

type bigint_lit = {positive: bool; value: string}

type number =
  | Float of float_lit
  | Int of {i: int32; c: int option}
  | BigInt of bigint_lit

(* becareful when constant folding +/-,
   since we treat it as js nativeint, bitwise operators:
   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators
   The operands of all bitwise operators are converted to signed 32-bit integers in two's complement format.'
*)

type mutable_flag = Mutable | Immutable | NA

type direction_flag = Upto | Downto | Up

(*
    {[
      let rec x = 1 :: y
      and y = 1 :: x
    ]}
*)
type recursive_info = SingleRecursive | NonRecursie | NA

type used_stats =
  | Dead_pure
  (* only [Dead] should be taken serious,
      other status can be converted during
      inlining
      -- all exported symbols can not be dead
      -- once a symbole is called Dead_pure,
      it can not be alive anymore, we should avoid iterating it
  *)
  | Dead_non_pure
  (* we still need iterating it,
     just its bindings does not make sense any more *)
  | Exported (* Once it's exported, shall we change its status anymore? *)
  (* In general, we should count in one pass, and eliminate code in another
     pass, you can not do it in a single pass, however, some simple
     dead code can be detected in a single pass
  *)
  | Once_pure
    (* used only once so that, if we do the inlining, it will be [Dead] *)
  | Used (**)
  | Scanning_pure
  | Scanning_non_pure
  | NA

type ident_info = {
  (* mutable recursive_info : recursive_info; *)
  mutable used_stats: used_stats;
}

type exports = Ident.t list

type tag_info = Lam_tag_info.t

type length_object = Array | String | Bytes | Function | Caml_block

(** TODO: define constant - for better constant folding  *)
(* type constant =  *)
(*   | Const_int of int *)
(*   | Const_ *)
