(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Types
open Lambda

let scrape_ty env ty =
  let ty = Ctype.expand_head_opt env (Ctype.correct_levels ty) in
  match ty.desc with
  | Tconstr (p, _, _) -> (
    match Env.find_type p env with
    | {type_unboxed = {unboxed = true; _}; _} -> (
      match Typedecl.get_unboxed_type_representation env ty with
      | None -> ty
      | Some ty2 -> ty2)
    | _ -> ty
    | exception Not_found -> ty)
  | _ -> ty

let scrape env ty = (scrape_ty env ty).desc

(**  [Types.constructor_description]
     records the type at the definition type so for ['a option]
     it will always be [Tvar]
*)
let rec type_cannot_contain_undefined (typ : Types.type_expr) (env : Env.t) =
  match scrape env typ with
  | Tconstr (p, _, _) -> (
    (* all built in types could not inhabit none-like values:
       int, char, float, bool, unit, exn, array, list, nativeint,
       int32, int64, bytes
    *)
    match Predef.type_is_builtin_path_but_option p with
    | For_sure_yes -> true
    | For_sure_no -> false
    | NA -> (
      let decl = Env.find_type p env in
      match decl.type_kind with
      | exception _ -> false
      | Type_abstract ->
        List.exists Typedecl.is_not_undefined_attr decl.type_attributes
      | Type_open -> false
      | Type_record _ -> true
      | Type_variant
          ( [
              {cd_id = {name = "None"}; cd_args = Cstr_tuple []};
              {cd_id = {name = "Some"}; cd_args = Cstr_tuple [_]};
            ]
          | [
              {cd_id = {name = "Some"}; cd_args = Cstr_tuple [_]};
              {cd_id = {name = "None"}; cd_args = Cstr_tuple []};
            ]
          | [{cd_id = {name = "()"}; cd_args = Cstr_tuple []}] ) ->
        false (* conservative *)
      | Type_variant cdecls ->
        let untagged =
          Ast_untagged_variants.has_untagged decl.type_attributes
        in
        Ext_list.for_all cdecls (fun cd ->
            if Ast_untagged_variants.has_undefined_literal cd.cd_attributes then
              false
            else if untagged then
              match cd.cd_args with
              | Cstr_tuple [t] ->
                Ast_untagged_variants.type_is_builtin_object t
                || type_cannot_contain_undefined t env
              | Cstr_tuple [] -> true
              | Cstr_tuple (_ :: _ :: _) ->
                true (* Not actually possible for untagged *)
              | Cstr_record [{ld_type = t}] ->
                Ast_untagged_variants.type_is_builtin_object t
                || type_cannot_contain_undefined t env
              | Cstr_record ([] | _ :: _ :: _) -> true
            else true)))
  | Ttuple _ | Tvariant _ | Tpackage _ | Tarrow _ -> true
  | Tfield _ | Tpoly _ | Tunivar _ | Tlink _ | Tsubst _ | Tnil | Tvar _
  | Tobject _ ->
    false

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (_, lhs, rhs, _, _) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr (p, _, _) -> Path.same p base_ty_path
  | _ -> false

let maybe_pointer_type env ty =
  if Ctype.maybe_pointer_type env ty then Pointer else Immediate
