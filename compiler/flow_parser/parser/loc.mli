(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = {
  line: int;
  column: int;
}
[@@deriving eq]

type t = {
  source: File_key.t option;
  start: position;
  _end: position;
}


val none : t

val btwn : t -> t -> t

val pos_cmp : position -> position -> int

val compare : t -> t -> int

val equal : t -> t -> bool [@@live]
(* Live: used by the equality [Token] derives with [@@deriving eq]. *)

(* Produces a location at the start of the input location *)
val start_loc : t -> t

(* Produces a location at the end of the input location *)
val end_loc : t -> t

