(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

let () =
  Ast_untagged_variants.extract_concrete_typedecl :=
    Ctype.extract_concrete_typedecl
let () = Ast_untagged_variants.expand_head := Ctype.expand_head

let names_from_construct_pattern (pat : Typedtree.pattern) =
  let rec resolve_path n (path : Path.t) =
    match Env.find_type path pat.pat_env with
    | {type_kind = Type_variant cstrs; _} ->
      Ast_untagged_variants.names_from_type_variant ~env:pat.pat_env cstrs
    | {type_kind = Type_abstract; type_manifest = Some t; _} -> (
      match (Ctype.unalias t).desc with
      | Tconstr (pathn, _, _) -> resolve_path (n + 1) pathn
      | _ -> None)
    | {type_kind = Type_abstract; type_manifest = None; _} -> None
    | {type_kind = Type_record _ | Type_open (* Exceptions *); _} -> None
  in

  match (Btype.repr pat.pat_type).desc with
  | Tconstr (path, _, _) -> resolve_path 0 path
  | _ -> assert false

(**
    Note it is a bit tricky when there is unbound var, 
    its type will be Tvar which is too complicated to support subtyping
*)
let variant_is_subtype (env : Env.t) (row_desc : Types.row_desc)
    (ty : Types.type_expr) : bool =
  match row_desc with
  | {
   row_closed = true;
   row_fixed = _;
   row_fields = (name, (Rabsent | Rpresent None)) :: rest;
  } ->
    if Ext_string.is_valid_hash_number name then
      Ext_list.for_all rest (function
        | name, (Rabsent | Rpresent None) ->
          Ext_string.is_valid_hash_number name
        | _ -> false)
      && Typeopt.is_base_type env ty Predef.path_int
    else
      Ext_list.for_all rest (function
        | name, (Rabsent | Rpresent None) ->
          not (Ext_string.is_valid_hash_number name)
        | _ -> false)
      && Typeopt.is_base_type env ty Predef.path_string
  | _ -> false
