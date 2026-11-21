(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

type t = Warnings.loc = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}

let in_file name =
  let loc = {pos_fname = name; pos_lnum = 1; pos_bol = 0; pos_cnum = -1} in
  {loc_start = loc; loc_end = loc; loc_ghost = true}

let none = in_file "_none_"

(* return file, line, char from the given position *)
let get_pos_info pos = (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

type 'a loc = {txt: 'a; loc: t}

let mkloc txt loc = {txt; loc}
let mknoloc txt = mkloc txt none

let map_loc f {txt; loc} = {txt = f txt; loc}
