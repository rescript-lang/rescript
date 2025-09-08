/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

type rec t =
  | Lident(string)
  | Ldot(t, string)
  | Lapply(t, t)

let rec flat = (accu, x) =>
  switch x {
  | Lident(s) => list{s, ...accu}
  | Ldot(lid, s) => flat(list{s, ...accu}, lid)
  | Lapply(_, _) => Misc.fatal_error("Longident.flat")
  }

let flatten = lid => flat(list{}, lid)

let last = x =>
  switch x {
  | Lident(s) => s
  | Ldot(_, s) => s
  | Lapply(_, _) => Misc.fatal_error("Longident.last")
  }

@raises(Invalid_argument)
let rec split_at_dots = (s, pos) =>
  try {
    let dot = String.index_from(s, pos, '.')
    list{String.sub(s, pos, dot - pos), ...split_at_dots(s, dot + 1)}
  } catch {
  | Not_found => list{String.sub(s, pos, String.length(s) - pos)}
  }

let unflatten = l =>
  switch l {
  | list{} => None
  | list{hd, ...tl} => Some(List.fold_left((p, s) => Ldot(p, s), Lident(hd), tl))
  }

@raises(Invalid_argument)
let parse = s =>
  switch unflatten(split_at_dots(s, 0)) {
  | None => Lident("") /* should not happen, but don't put assert false
   so as not to crash the toplevel (see Genprintval) */

  | Some(v) => v
  }

