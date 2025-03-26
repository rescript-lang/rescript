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

[@@@warning "+9"]

(* Refer https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
   for precedence
*)

let op_prec (op : Js_op.binop) =
  match op with
  | Eq -> (1, 13, 1)
  | Or -> (3, 3, 3)
  | And -> (4, 4, 4)
  | EqEqEq | NotEqEq -> (8, 8, 9)
  | Gt | Ge | Lt | Le | InstanceOf -> (9, 9, 10)
  | Bor -> (5, 5, 5)
  | Bxor -> (6, 6, 6)
  | Band -> (7, 7, 7)
  | Lsl | Lsr | Asr -> (10, 10, 11)
  | Plus | Minus -> (11, 11, 12)
  | Mul | Div | Mod -> (12, 12, 13)
  | Pow -> (13, 14, 12)

let op_int_prec (op : Js_op.int_op) =
  match op with
  | Bor -> (5, 5, 5)
  | Bxor -> (6, 6, 6)
  | Band -> (7, 7, 7)
  | Lsl | Lsr | Asr -> (10, 10, 11)
  | Plus | Minus -> (11, 11, 12)
  | Mul | Div | Mod -> (12, 12, 13)
  | Pow -> (13, 14, 12)

let op_str (op : Js_op.binop) =
  match op with
  | Bor -> "|"
  | Bxor -> "^"
  | Band -> "&"
  | Lsl -> "<<"
  | Lsr -> ">>>"
  | Asr -> ">>"
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | Eq -> "="
  | Or -> "||"
  | And -> "&&"
  | EqEqEq -> "==="
  | NotEqEq -> "!=="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | InstanceOf -> "instanceof"

let op_int_str (op : Js_op.int_op) =
  match op with
  | Bor -> "|"
  | Bxor -> "^"
  | Band -> "&"
  | Lsl -> "<<"
  | Lsr -> ">>>"
  | Asr -> ">>"
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

let str_of_used_stats x =
  match (x : Js_op.used_stats) with
  | Js_op.Dead_pure -> "Dead_pure"
  | Dead_non_pure -> "Dead_non_pure"
  | Exported -> "Exported"
  | Once_pure -> "Once_pure"
  | Used -> "Used"
  | Scanning_pure -> "Scanning_pure"
  | Scanning_non_pure -> "Scanning_non_pure"
  | NA -> "NA"

let update_used_stats (ident_info : J.ident_info) used_stats =
  match ident_info.used_stats with
  | Dead_pure | Dead_non_pure | Exported -> ()
  | Scanning_pure | Scanning_non_pure | Used | Once_pure | NA ->
    ident_info.used_stats <- used_stats

let same_str_opt (x : string option) (y : string option) =
  match (x, y) with
  | None, None -> true
  | Some x0, Some y0 -> x0 = y0
  | None, Some _ | Some _, None -> false

let same_vident (x : J.vident) (y : J.vident) =
  match (x, y) with
  | Id x0, Id y0 -> Ident.same x0 y0
  | Qualified (x, str_opt0), Qualified (y, str_opt1) ->
    let same_kind (x : Js_op.kind) (y : Js_op.kind) =
      match (x, y) with
      | Ml, Ml | Runtime, Runtime -> true
      | External {name = u; _}, External {name = v; _} ->
        u = v (* not comparing Default since we will do it later *)
      | _, _ -> false
    in
    Ident.same x.id y.id && same_kind x.kind y.kind
    && same_str_opt str_opt0 str_opt1
  | Id _, Qualified _ | Qualified _, Id _ -> false

let of_lam_mutable_flag (x : Asttypes.mutable_flag) : Js_op.mutable_flag =
  match x with
  | Immutable -> Immutable
  | Mutable -> Mutable
