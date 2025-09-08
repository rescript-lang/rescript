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

type t = Lexing.position = {
  pos_fname: string;
  pos_lnum: int;
  pos_bol: int;
  pos_cnum: int;
}

let offset (x : t) (y : t) =
  {
    x with
    pos_lnum = x.pos_lnum + y.pos_lnum - 1;
    pos_cnum = x.pos_cnum + y.pos_cnum;
    pos_bol = (if y.pos_lnum = 1 then x.pos_bol else x.pos_cnum + y.pos_bol);
  }

let print fmt (pos : t) =
  Format.fprintf fmt "(line %d, column %d)" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let lexbuf_from_channel_with_fname ic fname =
  let x = Lexing.from_function (fun buf n -> input ic buf 0 n) in
  let pos : t =
    {
      pos_fname = fname;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0 (* copied from zero_pos*);
    }
  in
  x.lex_start_p <- pos;
  x.lex_curr_p <- pos;
  x
