(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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

let ( // ) = Filename.concat

let normalize_exn (s : string) : string =
  let old_cwd = Sys.getcwd () in
  Unix.chdir s;
  let normalized = Sys.getcwd () in
  Unix.chdir old_cwd;
  normalized

let real_path p =
  match Sys.is_directory p with
  | exception _ ->
    let rec resolve dir =
      if Sys.file_exists dir then normalize_exn dir
      else
        let parent = Filename.dirname dir in
        if dir = parent then dir else resolve parent // Filename.basename dir
    in
    let p = if Filename.is_relative p then Sys.getcwd () // p else p in
    resolve p
  | true -> normalize_exn p
  | false -> (
    let dir = normalize_exn (Filename.dirname p) in
    match Filename.basename p with
    | "." -> dir
    | base -> dir // base)

let is_same_paths_via_io a b = if a = b then true else real_path a = real_path b
