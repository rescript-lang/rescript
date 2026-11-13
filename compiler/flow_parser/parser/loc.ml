(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* line numbers are 1-indexed; column numbers are 0-indexed *)
type position = {
  line: int;
  column: int;
}
[@@deriving eq]

(* start is inclusive; end is exclusive *)
(* If you are modifying this record, go look at ALoc.ml and make sure you understand the
 * representation there. *)
type t = {
  source: File_key.t option;
  start: position;
  _end: position;
}


let none = { source = None; start = { line = 0; column = 0 }; _end = { line = 0; column = 0 } }

let btwn loc1 loc2 = { source = loc1.source; start = loc1.start; _end = loc2._end }

let pos_cmp a b =
  let k = a.line - b.line in
  if k = 0 then
    a.column - b.column
  else
    k

let compare_ignore_source loc1 loc2 =
  match pos_cmp loc1.start loc2.start with
  | 0 -> pos_cmp loc1._end loc2._end
  | k -> k

let compare loc1 loc2 =
  let k = File_key.compare_opt loc1.source loc2.source in
  if k = 0 then
    compare_ignore_source loc1 loc2
  else
    k

(* Live: used by the equality [Token] derives with [@@deriving eq], whose
   generated references reanalyze does not see. *)
let[@live] equal loc1 loc2 = compare loc1 loc2 = 0

let start_loc loc = { loc with _end = loc.start }

let end_loc loc = { loc with start = loc._end }

