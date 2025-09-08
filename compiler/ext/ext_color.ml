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

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type style = FG of color | BG of color | Bold | Dim

(* let ansi_of_color = function
   | Black -> "0"
   | Red -> "1"
   | Green -> "2"
   | Yellow -> "3"
   | Blue -> "4"
   | Magenta -> "5"
   | Cyan -> "6"
   | White -> "7" *)

let code_of_style = function
  | FG Black -> "30"
  | FG Red -> "31"
  | FG Green -> "32"
  | FG Yellow -> "33"
  | FG Blue -> "34"
  | FG Magenta -> "35"
  | FG Cyan -> "36"
  | FG White -> "37"
  | BG Black -> "40"
  | BG Red -> "41"
  | BG Green -> "42"
  | BG Yellow -> "43"
  | BG Blue -> "44"
  | BG Magenta -> "45"
  | BG Cyan -> "46"
  | BG White -> "47"
  | Bold -> "1"
  | Dim -> "2"

(** TODO: add more styles later *)
let style_of_tag s =
  match s with
  | Format.String_tag "error" -> [Bold; FG Red]
  | Format.String_tag "warning" -> [Bold; FG Magenta]
  | Format.String_tag "info" -> [Bold; FG Yellow]
  | Format.String_tag "dim" -> [Dim]
  | Format.String_tag "filename" -> [FG Cyan]
  | _ -> []

let ansi_of_tag s =
  let l = style_of_tag s in
  let s = String.concat ";" (Ext_list.map l code_of_style) in
  "\x1b[" ^ s ^ "m"

let reset_lit = "\x1b[0m"
