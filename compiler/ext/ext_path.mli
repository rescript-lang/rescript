(* Copyright (C) 2017 Authors of ReScript
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

type t

val simple_convert_node_path_to_os_path : string -> string
(** Js_output is node style, which means
    separator is only '/'

    if the path contains 'node_modules',
    [node_relative_path] will discard its prefix and
    just treat it as a library instead
*)

val combine : string -> string -> string
(**
   [combine path1 path2]
   1. add some simplifications when concatenating
   2. when [path2] is absolute, return [path2]
*)

(**
   {[
     get_extension "a.txt" = ".txt"
       get_extension "a" = ""
   ]}
*)

val node_rebase_file : from:string -> to_:string -> string -> string

val rel_normalized_absolute_path : from:string -> string -> string
(**
   TODO: could be highly optimized
   if [from] and [to] resolve to the same path, a zero-length string is returned
   Given that two paths are directory

   A typical use case is
   {[
     Filename.concat
       (rel_normalized_absolute_path cwd (Filename.dirname a))
       (Filename.basename a)
   ]}
*)

val normalize_absolute_path : string -> string

val absolute_cwd_path : string -> string

val concat : string -> string -> string
(** [concat dirname filename]
    The same as {!Filename.concat} except a tiny optimization
    for current directory simplification
*)

val check_suffix_case : string -> string -> bool

(* It is lazy so that it will not hit errors when in script mode *)
val package_dir : string Lazy.t
