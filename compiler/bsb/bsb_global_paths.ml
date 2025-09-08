(* Copyright (C) 2019 - Hongbo Zhang, Authors of ReScript
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

let cwd = Sys.getcwd ()

(**
   If [Sys.executable_name] gives an absolute path, 
   nothing needs to be done.

   If [Sys.executable_name] is not an absolute path, for example
   (rlwrap ./ocaml)
   it is a relative path, 
   it needs be adapted based on cwd

   if [Sys.executable_name] gives an absolute path, 
   nothing needs to be done
   if it is a relative path 

   there are two cases: 
   - bsb.exe
   - ./bsb.exe 
     The first should also not be touched
     Only the latter need be adapted based on project root  
*)

let bsc_dir =
  Filename.dirname
    (Ext_path.normalize_absolute_path
       (Ext_path.combine cwd Sys.executable_name))

let vendor_bsc = Filename.concat bsc_dir "bsc.exe"

let vendor_ninja = Filename.concat bsc_dir "ninja.exe"

let vendor_bsdep = Filename.concat bsc_dir "bsb_helper.exe";;

assert (Sys.file_exists bsc_dir)
