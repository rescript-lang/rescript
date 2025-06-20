(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

val flag_concat : string -> string list -> string
(**
   Use:
   {[
     flag_concat "-ppx" [ppxs]
   ]}
*)

val ppx_flags : Bsb_config_types.ppx list -> string
(**
   Build quoted commandline arguments for bsc.exe for the given ppx flags

   Use:
   {[
     ppx_flags [ppxs]
   ]}
*)

val pp_flag : string -> string

val include_dirs : string list -> string
(**
   Build unquoted command line arguments for bsc.exe for the given include dirs

   Use:
   {[
     include_dirs [dirs]
   ]}
*)

val include_dirs_by : 'a list -> ('a -> string) -> string

val mkp : string -> unit

(* The path of [bsc] and [bsdep] is normalized so that the invokation of [./compiler/bin/bsb.exe]
   and [bsb.exe] (combined with a dirty rescript.json) will not trigger unnecessary rebuild.

   The location of [bsc] and [bsdep] is configured by the combination of [Sys.executable_name]
   and [cwd].

   In theory, we should also check the integrity of [bsb.exe], if it is changed, the rebuild
   should be regen, but that is too much in practice, not only you need check the integrity of
   path of [bsb.exe] but also the timestamp, to make it 100% correct, also the integrity of
   [bsdep.exe] [bsc.exe] etc.
*)

val get_list_string_acc : Ext_json_types.t array -> string list -> string list

val get_list_string : Ext_json_types.t array -> string list

type top = Expect_none | Expect_name of string

type result = {path: string; checked: bool}

(* [resolve_bsb_magic_file]
   returns a tuple (path,checked)
   when checked is true, it means such file should exist without depending on env
*)
val resolve_bsb_magic_file : cwd:string -> desc:string -> string -> result

type package_context = {proj_dir: string; top: top; is_pinned: bool}

val extract_pinned_dependencies : Ext_json_types.t Map_string.t -> Set_string.t

val walk_all_deps :
  string -> pinned_dependencies:Set_string.t -> package_context Queue.t
