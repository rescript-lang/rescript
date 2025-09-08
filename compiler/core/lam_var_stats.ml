(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

(** Used in loop, huge punishment *)
let loop_use = 100

type stats = {
  top: bool;
  (* all appearances are in the top,  substitution is fine
     whether it is pure or not
     {[
       (fun x y
         ->  x + y + (f x )) (32) (console.log('hi'), 33)
     ]}
     since in ocaml, the application order is intentionally undefined,
     note if [times] is not one, this field does not make sense
  *)
  times: int;
}

let fresh_stats : stats = {top = true; times = 0}

let sink_stats : stats = {top = false; times = loop_use}

(* let stats top times = {top; times} *)
let top_and_used_zero_or_one x =
  match x with
  | {top = true; times = 0 | 1} -> true
  | _ -> false

type position =
  | Begin (* top = true ; loop = false *)
  | Not_begin (* top = false; loop = false *)
  | Sink
(* loop = true *)

let update (v : stats) (pos : position) : stats =
  match pos with
  | Begin -> {v with times = v.times + 1}
  | Not_begin -> {top = false; times = v.times + 1}
  | Sink -> sink_stats

let sink : position = Sink

let fresh_env : position = Begin

(* no side effect, if argument has no side effect and used only once we can simply do the replacement *)
let new_position_after_lam lam (env : position) : position =
  if (not (env = Begin)) || Lam_analysis.no_side_effects lam then env
  else Not_begin
