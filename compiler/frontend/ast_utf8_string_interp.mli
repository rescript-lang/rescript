(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type kind = String | Var of int * int (* int records its border length *)

type error = private
  | Invalid_code_point
  | Unterminated_backslash
  | Invalid_escape_code of char
  | Invalid_hex_escape
  | Invalid_unicode_escape
  | Unterminated_variable
  | Unmatched_paren
  | Invalid_syntax_of_var of string

type pos = {lnum: int; offset: int; byte_bol: int}
(** Note the position is about code point *)

type segment = {start: pos; finish: pos; kind: kind; content: string}
type segments = segment list

type cxt = {
  mutable segment_start: pos;
  buf: Buffer.t;
  s_len: int;
  mutable segments: segments;
  mutable pos_bol: int;
  (* record the abs position of current beginning line *)
  mutable byte_bol: int;
  mutable pos_lnum: int; (* record the line number *)
}

type exn += Error of pos * pos * error

val empty_segment : segment -> bool
val transform_test : string -> segment list
val transform_exp :
  Parsetree.expression -> string -> string -> Parsetree.expression
val transform_pat : Parsetree.pattern -> string -> string -> Parsetree.pattern
val is_unicode_string : string -> bool
val is_unescaped : string -> bool
val parse_processed_delim : string option -> External_arg_spec.delim option
