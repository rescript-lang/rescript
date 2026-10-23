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
    (* Actually assignment.
       TODO: move it into statement, so that all expressions
       are side effect free (except function calls)
    *)
  | Or
  | And
  | EqEqEq
  | NotEqEq
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

type kind =
  | Ml
  | Runtime
  | External of {
      name: string;
      default: bool;
      import_attributes: External_ffi_types.import_attributes option;
    }

type property = Lambda.let_kind = Strict | Alias | StrictOpt | Variable

type property_name = Lit of string | Symbol_name

(* literal char *)
type float_lit = {f: string} [@@unboxed]

type bigint_lit = {positive: bool; value: string}

type number =
  | Float of float_lit
  | Int of {i: int32; c: int option}
  | BigInt of bigint_lit

(* Be careful when constant folding +/-,
   since we treat it as js nativeint, bitwise operators:
   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators
   The operands of all bitwise operators are converted to signed 32-bit integers in two's complement format.'
*)

type mutable_flag = Mutable | Immutable | NA

type direction_flag = Upto | Downto | Up

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

type ident_info = {mutable used_stats: used_stats}

type exports = Ident.t list

type tag_info = Lambda.tag_info
