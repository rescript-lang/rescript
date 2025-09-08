(* Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
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

let ( // ) = Filename.concat

type t = Bsb_pkg_types.t

let make_sub_path (x : t) : string =
  Literals.node_modules // Bsb_pkg_types.to_string x

let node_paths : string list Lazy.t =
  lazy
    (try
       Ext_string.split (Sys.getenv "NODE_PATH")
         (if Sys.win32 then ';' else ':')
     with _ -> [])

(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure
*)
let check_dir dir =
  match Sys.file_exists dir with
  | true -> Some dir
  | false -> None

let resolve_bs_package_aux ~cwd (pkg : t) =
  (* First try to resolve recursively from the current working directory  *)
  let sub_path = make_sub_path pkg in
  let rec aux cwd =
    let abs_marker = cwd // sub_path in
    if Sys.file_exists abs_marker then abs_marker
    else
      let another_cwd = Filename.dirname cwd in
      (* TODO: may non-terminating when see symlinks *)
      if String.length another_cwd < String.length cwd then aux another_cwd
      else
        (* To the end try other possiblilities [NODE_PATH]*)
        match
          Ext_list.find_opt (Lazy.force node_paths) (fun dir ->
              check_dir (dir // Bsb_pkg_types.to_string pkg))
        with
        | Some resolved_dir -> resolved_dir
        | None -> Bsb_exception.package_not_found ~pkg
  in
  aux cwd

module Coll = Hash.Make (struct
  type nonrec t = t

  let equal = Bsb_pkg_types.equal

  let hash (x : t) = Hashtbl.hash x
end)

let cache : string Coll.t = Coll.create 0

let to_list cb = Coll.to_list cache cb

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd (package : t) =
  match Coll.find_opt cache package with
  | None ->
    let result = resolve_bs_package_aux ~cwd package in
    Bsb_log.info "@{<info>Package@} %a -> %s@." Bsb_pkg_types.print package
      result;
    Coll.add cache package result;
    result
  | Some x ->
    let result = resolve_bs_package_aux ~cwd package in
    if not (Bsb_real_path.is_same_paths_via_io result x) then
      Bsb_log.warn
        "@{<warning>Duplicated package:@} %a %s (chosen) vs %s in %s @."
        Bsb_pkg_types.print package x result cwd;
    x
