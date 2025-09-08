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
type module_info = Bsb_db.module_info

type t = Bsb_db.map

let conflict_module_info modname (a : module_info) (b : module_info) =
  Bsb_exception.conflict_module modname a.dir b.dir

(* merge data info from two directories*)
let merge (acc : t) (sources : t) : t =
  Map_string.disjoint_merge_exn acc sources conflict_module_info

let sanity_check (map : t) =
  Map_string.iter map (fun m module_info ->
      if module_info.info = Intf then Bsb_exception.no_implementation m)

(* invariant check:
   ml and mli should have the same case, same path
*)
let check (x : module_info) name_sans_extension case (module_info : Bsb_db.info)
    =
  let x_ml_info = x.info in
  if
    x.name_sans_extension <> name_sans_extension
    || x.case <> case || x_ml_info = module_info || x_ml_info = Impl_intf
  then
    Bsb_exception.invalid_spec
      (Printf.sprintf
         "implementation and interface have different path names or different \
          cases %s vs %s"
         x.name_sans_extension name_sans_extension);
  x.info <- Impl_intf;
  x

let warning_unused_file : _ format =
  "@{<warning>IGNORED@}: file %s under %s is ignored because it can't be \
   turned into a valid module name. \n\
   The build system transforms a file name into a module name by upper-casing \
   the first letter@."
(* TODO: add a link for more explanations *)

let is_editor_temporary_files basename = Ext_string.starts_with basename ".#"

(*
   Example: .#hi.ml
   Note for other files like <filename>~, <filename>.swp
   it does not pass the suffix rules
*)
let add_basename ~(dir : string) (map : t) ?error_on_invalid_suffix basename : t
    =
  if is_editor_temporary_files basename then map
  else
    let info = ref Bsb_db.Impl in
    let invalid_suffix = ref false in
    let file_suffix = Ext_filename.get_extension_maybe basename in
    (match () with
    | _ when file_suffix = Literals.suffix_res -> ()
    | _ when file_suffix = Literals.suffix_resi -> info := Intf
    | _ -> invalid_suffix := true);
    let info = !info in
    let invalid_suffix = !invalid_suffix in
    if invalid_suffix then
      match error_on_invalid_suffix with
      | None -> map
      | Some loc -> Bsb_exception.errorf ~loc "invalid suffix %s" basename
    else
      match Ext_filename.as_module ~basename:(Filename.basename basename) with
      | None ->
        Bsb_log.warn warning_unused_file basename dir;
        map
      | Some {module_name; case} ->
        let name_sans_extension =
          Filename.concat dir (Ext_filename.chop_extension_maybe basename)
        in
        let dir = Filename.dirname name_sans_extension in
        Map_string.adjust map module_name (fun opt_module_info ->
            match opt_module_info with
            | None -> {dir; name_sans_extension; info; case}
            | Some x -> check x name_sans_extension case info)
