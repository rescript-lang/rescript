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
    | {type_representation = Unboxed; _} -> (
      match Ctype.get_unboxed_type_representation env ty with
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
let rec cannot_contain_undefined ~visiting (typ : Types.type_expr) (env : Env.t)
    =
  (* A value declared to be a class instance or to have a [typeof] other than
     "undefined" is not undefined. The payload of an [@unboxed] wrapper is
     declared only after scraping. *)
  Variant_layout.value_shape ~env typ <> Unstated
  ||
  let scraped = scrape_ty env typ in
  Variant_layout.value_shape ~env scraped <> Unstated
  ||
  match scraped.desc with
  | Tconstr (p, _, _) -> (
    (* all built in types could not inhabit none-like values:
       int, char, float, bool, unit, exn, array, list, nativeint,
       int32, int64, bytes
    *)
    match Predef.type_is_builtin_path_but_option p with
    | For_sure_yes -> true
    | For_sure_no -> false
    | NA -> (
      match Env.find_type p env with
      | exception Not_found ->
        (* A transitive dependency's interface may not be on the search path.
           Keep option wrapping when its runtime representation is unknown. *)
        false
      | {type_kind = Type_abstract} ->
        (* Its shape was read from the scraped type above *)
        false
      | {type_kind = Type_open} -> false
      | {type_kind = Type_record _} -> true
      | {
       type_kind =
         Type_variant
           ( ( [
                 {cd_id = {name = "None"}; cd_args = Cstr_tuple []};
                 {cd_id = {name = "Some"}; cd_args = Cstr_tuple [_]};
               ]
             | [
                 {cd_id = {name = "Some"}; cd_args = Cstr_tuple [_]};
                 {cd_id = {name = "None"}; cd_args = Cstr_tuple []};
               ]
             | [{cd_id = {name = "()"}; cd_args = Cstr_tuple []}] ),
             _ );
      } ->
        false (* conservative *)
      | {type_kind = Type_variant _} when List.exists (Path.same p) visiting ->
        (* A value reaches [undefined] through finitely many unboxed
           payloads, so the cases met again add none: whether the variant
           can be [undefined] is decided by its other cases *)
        true
      | {type_kind = Type_variant (cdecls, layout_ref)} ->
        let visiting = p :: visiting in
        let layout = Variant_runtime.get_layout layout_ref in
        let rec all_cases_cannot_contain_undefined position = function
          | [] -> true
          | cd :: rest ->
            let case = Variant_runtime.constructor_at layout position in
            let tag, payload_is_unboxed =
              match case with
              | Variant_runtime.Constant tag -> (tag, false)
              | Variant_runtime.Block (Tagged {tag}) -> (tag, false)
              | Variant_runtime.Block (Untagged {tag}) -> (tag, true)
            in
            tag.literal <> Variant_runtime.Undefined
            && ((not payload_is_unboxed)
               ||
               match cd.cd_args with
               | Cstr_tuple [t] -> cannot_contain_undefined ~visiting t env
               | Cstr_tuple [] -> true
               | Cstr_tuple (_ :: _ :: _) ->
                 true (* Not actually possible for an unboxed payload. *)
               | Cstr_record [{ld_type = t}] ->
                 cannot_contain_undefined ~visiting t env
               | Cstr_record ([] | _ :: _ :: _) -> true)
            && all_cases_cannot_contain_undefined (position + 1) rest
        in
        all_cases_cannot_contain_undefined 0 cdecls))
  | Ttuple _ | Tvariant _ | Tpackage _ | Tarrow _ -> true
  | Tfield _ | Tpoly _ | Tunivar _ | Tlink _ | Tsubst _ | Tnil | Tvar _
  | Tobject _ ->
    false

let type_cannot_contain_undefined typ env =
  cannot_contain_undefined ~visiting:[] typ env

type abstractness = Opaque | Stands_for_another | Not_abstract

(* Whether [ty] is, also through abbreviations, an abstract type with a
   representation of its own, which an annotation can refine, or one that
   stands for another type: a functor parameter's type, a recursive module's
   type being defined, or a locally abstract type. *)
let abstractness env ty =
  match (Ctype.expand_head_opt env ty).desc with
  | Tconstr (path, _, _) -> (
    match Env.find_type path env with
    | {type_kind = Type_abstract; type_manifest = None; type_newtype_level} ->
      if type_newtype_level <> None || Env.is_functor_arg path env then
        Stands_for_another
      else if Predef.type_is_builtin_path_but_option path <> Predef.NA then
        Not_abstract
      else Opaque
    | _ -> Not_abstract
    | exception Not_found -> Not_abstract)
  | _ -> Not_abstract

type undefinedness =
  | Cannot_be_undefined
  | Unseen
  | Stands_for_another
  | Can_be_undefined

(* Whether a type can hold [undefined], as far as the compiler sees. An
   opaque abstract type or a JavaScript object type has a representation the
   compiler does not see. *)
let undefinedness env ty : undefinedness =
  if type_cannot_contain_undefined ty env then Cannot_be_undefined
  else
    match abstractness env ty with
    | Opaque -> Unseen
    | Stands_for_another -> Stands_for_another
    | Not_abstract -> (
      match (Ctype.expand_head_opt env ty).desc with
      | Tobject _ -> Unseen
      | _ -> Can_be_undefined)

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (params, ret) -> Some ((List.hd params).typ, ret)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr (p, _, _) -> Path.same p base_ty_path
  | _ -> false

let maybe_pointer_type env ty =
  if Ctype.maybe_pointer_type env ty then Pointer else Immediate
