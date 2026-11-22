(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Consistency tables: for checking consistency of module CRCs *)

type t = (string, Digest.t * string) Hashtbl.t

let create () = Hashtbl.create 13

let clear = Hashtbl.clear

exception Inconsistency of string * string * string

let check tbl name crc source =
  try
    let old_crc, old_source = Hashtbl.find tbl name in
    if crc <> old_crc then raise (Inconsistency (name, source, old_source))
  with Not_found -> Hashtbl.add tbl name (crc, source)

let set tbl name crc source = Hashtbl.add tbl name (crc, source)

let extract l tbl =
  let l = List.sort_uniq String.compare l in
  List.fold_left
    (fun assc name ->
      try
        let crc, _ = Hashtbl.find tbl name in
        (name, Some crc) :: assc
      with Not_found -> (name, None) :: assc)
    [] l
