(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

(* [@@@warning "-37"] *)
type t =
  (* | File of string  *)
  | Dir of string
[@@unboxed]

let cwd = lazy (Sys.getcwd ())

let split_by_sep_per_os : string -> string list =
  if Ext_sys.is_windows_or_cygwin then fun x ->
    (* on Windows, we can still accept -bs-package-output lib/js *)
    Ext_string.split_by
      (fun x ->
        match x with
        | '/' | '\\' -> true
        | _ -> false)
      x
  else fun x -> Ext_string.split x '/'

let node_relative_path ~from:(file_or_dir_2 : t) (file_or_dir_1 : t) =
  let relevant_dir1 =
    match file_or_dir_1 with
    | Dir x -> x
    (* | File file1 ->  Filename.dirname file1 *)
  in
  let relevant_dir2 =
    match file_or_dir_2 with
    | Dir x -> x
    (* | File file2 -> Filename.dirname file2  *)
  in
  let dir1 = split_by_sep_per_os relevant_dir1 in
  let dir2 = split_by_sep_per_os relevant_dir2 in
  let rec go (dir1 : string list) (dir2 : string list) =
    match (dir1, dir2) with
    | "." :: xs, ys -> go xs ys
    | xs, "." :: ys -> go xs ys
    | x :: xs, y :: ys when x = y -> go xs ys
    | _, _ -> Ext_list.map_append dir2 dir1 (fun _ -> Literals.node_parent)
  in
  match go dir1 dir2 with
  | x :: _ as ys when x = Literals.node_parent ->
    String.concat Literals.node_sep ys
  | ys -> String.concat Literals.node_sep @@ (Literals.node_current :: ys)

let node_concat ~dir base = dir ^ Literals.node_sep ^ base

let node_rebase_file ~from ~to_ file =
  node_concat
    ~dir:
      (if from = to_ then Literals.node_current
       else node_relative_path ~from:(Dir from) (Dir to_))
    file

let ( // ) x y =
  if x = Filename.current_dir_name then y
  else if y = Filename.current_dir_name then x
  else Filename.concat x y

(**
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned

   This function is useed in [es6-global] and
   [amdjs-global] format and tailored for `rollup`
*)

let absolute_path cwd s =
  let process s =
    let s = if Filename.is_relative s then Lazy.force cwd // s else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base, dir = (Filename.basename s, Filename.dirname s) in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in
    aux s
  in
  process s

let absolute_cwd_path s = absolute_path cwd s

(* let absolute cwd s =
   match s with
   | File x -> File (absolute_path cwd x )
   | Dir x -> Dir (absolute_path cwd x) *)

(* Input must be absolute directory *)
let rec find_root_filename ~cwd filenames =
  let file_exists =
    Ext_list.exists filenames (fun filename ->
        Sys.file_exists (Filename.concat cwd filename))
  in
  if file_exists then cwd
  else
    let cwd' = Filename.dirname cwd in
    if String.length cwd' < String.length cwd then
      find_root_filename ~cwd:cwd' filenames
    else
      Ext_fmt.failwithf ~loc:__LOC__ "%s not found from %s" (List.hd filenames)
        cwd

let find_config_dir cwd = find_root_filename ~cwd [Literals.rescript_json]

let package_dir = lazy (find_config_dir (Lazy.force cwd))
