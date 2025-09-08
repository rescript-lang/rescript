(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2016 - Hongbo Zhang, Authors of ReScript
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

let find_in_path_uncap path name =
  let uname = Ext_string.uncapitalize_ascii name in
  let rec try_dir = function
    | [] -> None
    | dir :: rem ->
      let ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then Some ufullname
      else
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then Some fullname else try_dir rem
  in
  try_dir path

(* ATTENTION: lazy to wait [Config.load_path] populated *)
let find_opt file = find_in_path_uncap !Config.load_path file

let output_prefix name =
  match !Clflags.output_name with
  | None ->
    Ext_namespace_encode.make
      (Filename.remove_extension name)
      ?ns:!Clflags.dont_record_crc_unit
  | Some oname -> Filename.remove_extension oname
