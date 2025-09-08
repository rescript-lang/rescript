(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

let await = "await"

let function_ ~async ~arrow =
  match (async, arrow) with
  | true, true -> "async "
  | false, true -> ""
  | true, false -> "async function "
  | false, false -> "function "

let arrow = "=>"

let let_ = "let"

let return = "return"

(* let eq = "=" *)
let require = "require"

let import = "import"

let from = "from"

let as_ = "as"

let export = "export"

let star = "*"

let lparen = "("

let rparen = ")"

let exports = "exports"

let dot = "."

let comma = ","

let colon = Ext_string.single_colon

let colon_space = ": "

let throw = "throw"

let default = "default"

let length = "length"

let code_point_at = "codePointAt"

let new_ = "new"

let array = "Array"

let question = "?"

let plusplus = "++"

let minusminus = "--"

let semi = ";"

let else_ = "else"

let if_ = "if"

let for_ = "for"

let try_ = "try"

let finally = "finally"

let this = "this"

let while_ = "while"

let empty_block = "empty_block"

let start_block = "start_block"

let end_block = "end_block"

let json = "JSON"

let stringify = "stringify"

let console = "console"

let define = "define"

let break = "break"

let continue = "continue"

let switch = "switch"

let strict_directive = "'use strict';"

let true_ = "true"

let false_ = "false"

let debugger = "debugger"

let tag = "TAG"

let bind = "bind"

let math = "Math"

let apply = "apply"

let null = "null"

let undefined = "undefined"

let string_cap = "String"

let from_charcode = "fromCharCode"

let eq = "="

let le = "<="

let lt = "<"

let ge = ">="

let gt = ">"

let plus_plus = "++"

(*  FIXME: use (i = i + 1 | 0) instead  *)
let minus_minus = "--"

let caml_block_create = "__"

(** debug symbols *)

let block_poly_var = "polyVar"

let block_variant = "variant"

let block_simple_variant = "simpleVariant"

let case = "case"
