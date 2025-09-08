(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

type anon_fun = rev_args:string list -> unit

type string_action =
  | String_call of (string -> unit)
  | String_set of string ref
  | String_optional_set of string option ref
  | String_list_add of string list ref

type unit_action =
  | Unit_call of (unit -> unit)
  | Unit_lazy of unit lazy_t
  | Unit_set of bool ref
  | Unit_clear of bool ref

type spec = Unit_dummy | Unit of unit_action | String of string_action

exception Bad = Arg.Bad

let bad_arg s = raise_notrace (Bad s)

type error = Unknown of string | Missing of string

type t = spec Ext_spec.t

let ( +> ) = Ext_buffer.add_string

let usage_b (buf : Ext_buffer.t) ~usage (speclist : t) =
  buf +> usage;
  buf +> "\nOptions:\n";
  let max_col = ref 0 in
  Ext_array.iter speclist (fun (key, _, _) ->
      if String.length key > !max_col then max_col := String.length key);
  Ext_array.iter speclist (fun (key, _, doc) ->
      if not (Ext_string.starts_with doc "*internal*") then (
        buf +> "  ";
        buf +> key;
        buf +> String.make (!max_col - String.length key + 2) ' ';
        let cur = ref 0 in
        let doc_length = String.length doc in
        while !cur < doc_length do
          match String.index_from_opt doc !cur '\n' with
          | None ->
            if !cur <> 0 then (
              buf +> "\n";
              buf +> String.make (!max_col + 4) ' ');
            buf +> String.sub doc !cur (String.length doc - !cur);
            cur := doc_length
          | Some new_line_pos ->
            if !cur <> 0 then (
              buf +> "\n";
              buf +> String.make (!max_col + 4) ' ');
            buf +> String.sub doc !cur (new_line_pos - !cur);
            cur := new_line_pos + 1
        done;
        buf +> "\n"))

let stop_raise ~usage ~(error : error) (speclist : t) =
  let b = Ext_buffer.create 200 in
  (match error with
  | Unknown ("-help" | "--help" | "-h") ->
    usage_b b ~usage speclist;
    Ext_buffer.output_buffer stdout b;
    exit 0
  | Unknown s ->
    b +> "Unknown option \"";
    b +> s;
    b +> "\".\n"
  | Missing s ->
    b +> "Option \"";
    b +> s;
    b +> "\" needs an argument.\n");
  usage_b b ~usage speclist;
  bad_arg (Ext_buffer.contents b)

let parse_exn ~usage ~argv ?(start = 1) ?(finish = Array.length argv)
    (speclist : t) (anonfun : rev_args:string list -> unit) =
  let current = ref start in
  let rev_list = ref [] in
  while !current < finish do
    let s = argv.(!current) in
    incr current;
    if s <> "" && s.[0] = '-' then
      match Ext_spec.assoc3 speclist s with
      | Some action -> (
        match action with
        | Unit_dummy -> ()
        | Unit r -> (
          match r with
          | Unit_set r -> r := true
          | Unit_clear r -> r := false
          | Unit_call f -> f ()
          | Unit_lazy f -> Lazy.force f)
        | String f -> (
          if !current >= finish then
            stop_raise ~usage ~error:(Missing s) speclist
          else
            let arg = argv.(!current) in
            incr current;
            match f with
            | String_call f -> f arg
            | String_set u -> u := arg
            | String_optional_set s -> s := Some arg
            | String_list_add s -> s := arg :: !s))
      | None -> stop_raise ~usage ~error:(Unknown s) speclist
    else rev_list := s :: !rev_list
  done;
  anonfun ~rev_args:!rev_list
