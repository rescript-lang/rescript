(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
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

type iterator = Ast_iterator.iterator

let super = Ast_iterator.default_iterator

let rec iter_warnings_on_stru (stru : Parsetree.structure) =
  match stru with
  | [] -> ()
  | head :: rest -> (
    match head.pstr_desc with
    | Pstr_attribute attr ->
      Builtin_attributes.warning_attribute attr;
      iter_warnings_on_stru rest
    | _ -> ())

let rec iter_warnings_on_sigi (stru : Parsetree.signature) =
  match stru with
  | [] -> ()
  | head :: rest -> (
    match head.psig_desc with
    | Psig_attribute attr ->
      Builtin_attributes.warning_attribute attr;
      iter_warnings_on_sigi rest
    | _ -> ())

let emit_external_warnings_on_structure (stru : Parsetree.structure) =
  Ast_checks.iterator.structure Ast_checks.iterator stru

let emit_external_warnings_on_signature (sigi : Parsetree.signature) =
  Ast_checks.iterator.signature Ast_checks.iterator sigi

(* The PPX bridge renames the [ocaml.]-prefixed builtins a PPX emits to their
   bare names, so source written with them is checked before it reaches the
   bridge. *)
let removed_ocaml_spellings : iterator =
  {
    super with
    attribute =
      (fun self attr ->
        Ast_checks.check_removed_ocaml_attribute attr;
        super.attribute self attr);
    extension =
      (fun self ext ->
        Ast_checks.check_removed_ocaml_extension ext;
        super.extension self ext);
  }

let check_removed_ocaml_spellings_on_structure (stru : Parsetree.structure) =
  removed_ocaml_spellings.structure removed_ocaml_spellings stru

let check_removed_ocaml_spellings_on_signature (sigi : Parsetree.signature) =
  removed_ocaml_spellings.signature removed_ocaml_spellings sigi
