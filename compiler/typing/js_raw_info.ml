(* Copyright (C) 2020 Hongbo Zhang, Authors of ReScript
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

type exp =
  | Js_function of {arity: int; arrow: bool}
  | Js_literal of {comment: string option}
  (* A special handling of
     [%raw "/*lint*/ 0"]
  *)
  (* Flow ast module
     {[
       and value =
           | String of string
         | Boolean of bool
         | Null
         | Number of float
         | BigInt of float
         | RegExp of RegExp.t
     ]}
  *)
  | Js_exp_unknown

type raw_kind = Raw_re | Raw_exp | Raw_program

type stmt = Js_stmt_comment | Js_stmt_unknown

type code_info = Exp of exp | Stmt of stmt

type t = {code: string; code_info: code_info}
