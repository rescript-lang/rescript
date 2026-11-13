(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The file a location belongs to. ReScript only parses strings, never files,
   so there is none: locations always have the source [None]. *)
type t = |

let compare_opt (a : t option) (b : t option) =
  match (a, b) with
  | (None, None) -> 0
  | (Some _, _)
  | (_, Some _) ->
    .

let check_suffix (file : t) (_ : string) =
  match file with
  | _ -> .
