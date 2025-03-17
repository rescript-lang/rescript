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
open Asttypes
open Typedtree
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
       int32, int64, lazy_t, bytes
    *)
    match Predef.type_is_builtin_path_but_option p with
    | For_sure_yes -> true
    | For_sure_no -> false
    | NA -> (
      let untagged = ref false in
      match
        let decl = Env.find_type p env in
        let () =
          if Ast_untagged_variants.has_untagged decl.type_attributes then
            untagged := true
        in
        decl.type_kind
      with
      | exception _ -> false
      | Type_abstract | Type_open -> false
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
        Ext_list.for_all cdecls (fun cd ->
            if Ast_untagged_variants.has_undefined_literal cd.cd_attributes then
              false
            else if !untagged then
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

type classification =
  | Int
  | Float
  | Lazy
  | Addr (* anything except a float or a lazy *)
  | Any

let classify env ty =
  let ty = scrape_ty env ty in
  if maybe_pointer_type env ty = Immediate then Int
  else
    match ty.desc with
    | Tvar _ | Tunivar _ -> Any
    | Tconstr (p, _args, _abbrev) -> (
      if Path.same p Predef.path_float then Float
      else if Path.same p Predef.path_lazy_t then Lazy
      else if Path.same p Predef.path_string || Path.same p Predef.path_array
      then Addr
      else
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract -> Any
          | Type_record _ | Type_variant _ | Type_open -> Addr
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any)
    | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ -> Addr
    | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ -> assert false

(** Whether a forward block is needed for a lazy thunk on a value, i.e.
    if the value can be represented as a float/forward/lazy *)
let lazy_val_requires_forward env ty =
  match classify env ty with
  | Any | Lazy -> true
  | Float (*-> Config.flat_float_array*) | Addr | Int -> false

(** The compilation of the expression [lazy e] depends on the form of e:
    constants, floats and identifiers are optimized.  The optimization must be
    taken into account when determining whether a recursive binding is safe. *)
let classify_lazy_argument :
    Typedtree.expression ->
    [ `Constant_or_function
    | `Float
    | `Identifier of [`Forward_value | `Other]
    | `Other ] =
 fun e ->
  match e.exp_desc with
  | Texp_constant
      ( Const_int _ | Const_char _ | Const_string _ | Const_int32 _
      | Const_int64 _ | Const_bigint _ )
  | Texp_function _
  | Texp_construct (_, {cstr_arity = 0}, _) ->
    `Constant_or_function
  | Texp_constant (Const_float _) -> `Float
  | Texp_ident _ when lazy_val_requires_forward e.exp_env e.exp_type ->
    `Identifier `Forward_value
  | Texp_ident _ -> `Identifier `Other
  | _ -> `Other
