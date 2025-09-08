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

type t = Lident of string | Ldot of t * string | Lapply of t * t
let rec cmp : t -> t -> int =
 fun a b ->
  if a == b then 0
  else
    match (a, b) with
    | Lident a, Lident b -> compare a b
    | Lident _, _ -> -1
    | _, Lident _ -> 1
    | Ldot (a, b), Ldot (c, d) -> (
      match cmp a c with
      | 0 -> compare b d
      | n -> n)
    | Ldot _, _ -> -1
    | _, Ldot _ -> 1
    | Lapply (a, b), Lapply (c, d) -> (
      match cmp a c with
      | 0 -> cmp b d
      | n -> n)

let rec flat accu = function
  | Lident s -> s :: accu
  | Ldot (lid, s) -> flat (s :: accu) lid
  | Lapply (_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
  | Lident s -> s
  | Ldot (_, s) -> s
  | Lapply (_, _) -> Misc.fatal_error "Longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found -> [String.sub s pos (String.length s - pos)]

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl -> Some (List.fold_left (fun p s -> Ldot (p, s)) (Lident hd) tl)

let parse s =
  match unflatten (split_at_dots s 0) with
  | None ->
    Lident ""
    (* should not happen, but don't put assert false
       so as not to crash the toplevel (see Genprintval) *)
  | Some v -> v
