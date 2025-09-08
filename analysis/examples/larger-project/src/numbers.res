/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Pierre Chambart, OCamlPro */
/* Mark Shinwell and Leo White, Jane Street Europe */
/*  */
/* Copyright 2013--2016 OCamlPro SAS */
/* Copyright 2014--2016 Jane Street Group LLC */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

module Int_base = {}

module Int = {}

module Int8 = {
  type t = int

  let zero = 0
  let one = 1

  let of_int_exn = i => i

  let to_int = i => i
}

module Int16 = {
  type t = int

  let of_int_exn = i => i

  let lower_int64 = Int64.neg(Int64.shift_left(Int64.one, 15))
  let upper_int64 = Int64.sub(Int64.shift_left(Int64.one, 15), Int64.one)

  let of_int64_exn = i => Int64.to_int(i)

  let to_int = t => t
}

module Float = {
  type t = float
}

