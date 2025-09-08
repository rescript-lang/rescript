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

type public = Export_none | Export_all | Export_set of Set_string.t

type build_generator = {
  input: string list;
  output: string list;
  command: string;
}

type file_group = {
  dir: string;
  sources: Bsb_db.map;
  resources: string list;
  public: public;
  is_dev: bool;
  generators: build_generator list;
      (* output of [generators] should be added to [sources],
         if it is [.ml,.mli,.res,.resi]
      *)
}

type file_groups = file_group list

type t = {files: file_groups; globbed_dirs: string list}

let empty : t = {files = []; globbed_dirs = []}

let merge (u : t) (v : t) =
  if u == empty then v
  else if v == empty then u
  else
    {
      files = Ext_list.append u.files v.files;
      globbed_dirs = Ext_list.append u.globbed_dirs v.globbed_dirs;
    }

let cons ~file_group ?globbed_dir (v : t) : t =
  {
    files = file_group :: v.files;
    globbed_dirs =
      (match globbed_dir with
      | None -> v.globbed_dirs
      | Some f -> f :: v.globbed_dirs);
  }

(** when [is_empty file_group]
    we don't need issue [-I] [-S] in [.merlin] file
*)
let is_empty (x : file_group) =
  Map_string.is_empty x.sources && x.resources = [] && x.generators = []
