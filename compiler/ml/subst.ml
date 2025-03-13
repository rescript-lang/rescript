(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Substitutions *)

open Misc
open Path
open Types
open Btype

type type_replacement =
  | Path of Path.t
  | Type_function of {params: type_expr list; body: type_expr}

module PathMap = Map.Make (Path)

type t = {
  types: type_replacement PathMap.t;
  modules: Path.t PathMap.t;
  modtypes: (Ident.t, module_type) Tbl.t;
  for_saving: bool;
}

let identity =
  {
    types = PathMap.empty;
    modules = PathMap.empty;
    modtypes = Tbl.empty;
    for_saving = false;
  }

let add_type_path id p s = {s with types = PathMap.add id (Path p) s.types}
let add_type id p s = add_type_path (Pident id) p s

let add_type_function id ~params ~body s =
  {s with types = PathMap.add id (Type_function {params; body}) s.types}

let add_module_path id p s = {s with modules = PathMap.add id p s.modules}
let add_module id p s = add_module_path (Pident id) p s

let add_modtype id ty s = {s with modtypes = Tbl.add id ty s.modtypes}

let for_saving s = {s with for_saving = true}

let loc s x =
  if s.for_saving && not !Clflags.keep_locs then Location.none else x

let remove_loc =
  let open Ast_mapper in
  {default_mapper with location = (fun _this _loc -> Location.none)}

let attrs s x =
  if s.for_saving && not !Clflags.keep_locs then
    remove_loc.Ast_mapper.attributes remove_loc x
  else x

let rec module_path s path =
  try PathMap.find path s.modules
  with Not_found -> (
    match path with
    | Pident _ -> path
    | Pdot (p, n, pos) -> Pdot (module_path s p, n, pos)
    | Papply (p1, p2) -> Papply (module_path s p1, module_path s p2))

let modtype_path s = function
  | Pident id as p -> (
    try
      match Tbl.find id s.modtypes with
      | Mty_ident p -> p
      | _ -> fatal_error "Subst.modtype_path"
    with Not_found -> p)
  | Pdot (p, n, pos) -> Pdot (module_path s p, n, pos)
  | Papply _ -> fatal_error "Subst.modtype_path"

let type_path s path =
  match PathMap.find path s.types with
  | Path p -> p
  | Type_function _ -> assert false
  | exception Not_found -> (
    match path with
    | Pident _ -> path
    | Pdot (p, n, pos) -> Pdot (module_path s p, n, pos)
    | Papply _ -> fatal_error "Subst.type_path")

let type_path s p =
  match Path.constructor_typath p with
  | Regular p -> type_path s p
  | Cstr (ty_path, cstr) -> Pdot (type_path s ty_path, cstr, nopos)
  | LocalExt _ -> type_path s p
  | Ext (p, cstr) -> Pdot (module_path s p, cstr, nopos)

let to_subst_by_type_function s p =
  match PathMap.find p s.types with
  | Path _ -> false
  | Type_function _ -> true
  | exception Not_found -> false

(* Special type ids for saved signatures *)

let new_id = ref (-1)
let reset_for_saving () = new_id := -1

let newpersty desc =
  decr new_id;
  {desc; level = generic_level; id = !new_id}

(* ensure that all occurrences of 'Tvar None' are physically shared *)
let tvar_none = Tvar None
let tunivar_none = Tunivar None
let norm = function
  | Tvar None -> tvar_none
  | Tunivar None -> tunivar_none
  | d -> d

let ctype_apply_env_empty = ref (fun _ -> assert false)

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  match ty.desc with
  | (Tvar _ | Tunivar _) as desc ->
    if s.for_saving || ty.id < 0 then (
      let ty' =
        if s.for_saving then newpersty (norm desc) else newty2 ty.level desc
      in
      save_desc ty desc;
      ty.desc <- Tsubst ty';
      ty')
    else ty
  | Tsubst ty -> ty
  | Tfield (m, k, _t1, _t2)
    when (not s.for_saving) && m = dummy_method
         && field_kind_repr k <> Fabsent
         && (repr ty).level < generic_level ->
    (* do not copy the type of self when it is not generalized *)
    ty
  (* cannot do it, since it would omit substitution
     | Tvariant row when not (static_row row) ->
         ty
  *)
  | _ ->
    let desc = ty.desc in
    save_desc ty desc;
    let tm = row_of_type ty in
    let has_fixed_row =
      (not (is_Tconstr ty)) && is_constr_row ~allow_ident:false tm
    in
    (* Make a stub *)
    let ty' = if s.for_saving then newpersty (Tvar None) else newgenvar () in
    ty.desc <- Tsubst ty';
    ty'.desc <-
      (if has_fixed_row then
         match tm.desc with
         (* PR#7348 *)
         | Tconstr (Pdot (m, i, pos), tl, _abbrev) ->
           let i' = String.sub i 0 (String.length i - 4) in
           Tconstr (type_path s (Pdot (m, i', pos)), tl, ref Mnil)
         | _ -> assert false
       else
         match desc with
         | Tconstr (p, args, _abbrev) -> (
           let args = List.map (typexp s) args in
           match PathMap.find p s.types with
           | exception Not_found -> Tconstr (type_path s p, args, ref Mnil)
           | Path _ -> Tconstr (type_path s p, args, ref Mnil)
           | Type_function {params; body} ->
             (!ctype_apply_env_empty params body args).desc)
         | Tpackage (p, n, tl) ->
           Tpackage (modtype_path s p, n, List.map (typexp s) tl)
         | Tobject (t1, name) ->
           Tobject
             ( typexp s t1,
               ref
                 (match !name with
                 | None -> None
                 | Some (p, tl) ->
                   if to_subst_by_type_function s p then None
                   else Some (type_path s p, List.map (typexp s) tl)) )
         | Tvariant row -> (
           let row = row_repr row in
           let more = repr row.row_more in
           (* We must substitute in a subtle way *)
           (* Tsubst takes a tuple containing the row var and the variant *)
           match more.desc with
           | Tsubst {desc = Ttuple [_; ty2]} ->
             (* This variant type has been already copied *)
             ty.desc <- Tsubst ty2;
             (* avoid Tlink in the new type *)
             Tlink ty2
           | _ -> (
             let dup =
               s.for_saving || more.level = generic_level || static_row row
               ||
               match more.desc with
               | Tconstr _ -> true
               | _ -> false
             in
             (* Various cases for the row variable *)
             let more' =
               match more.desc with
               | Tsubst ty -> ty
               | Tconstr _ | Tnil -> typexp s more
               | Tunivar _ | Tvar _ ->
                 save_desc more more.desc;
                 if s.for_saving then newpersty (norm more.desc)
                 else if dup && is_Tvar more then newgenty more.desc
                 else more
               | _ -> assert false
             in
             (* Register new type first for recursion *)
             more.desc <- Tsubst (newgenty (Ttuple [more'; ty']));
             (* Return a new copy *)
             let row = copy_row (typexp s) true row (not dup) more' in
             match row.row_name with
             | Some (p, tl) ->
               Tvariant
                 {
                   row with
                   row_name =
                     (if to_subst_by_type_function s p then None
                      else Some (type_path s p, tl));
                 }
             | None -> Tvariant row))
         | Tfield (_label, kind, _t1, t2) when field_kind_repr kind = Fabsent ->
           Tlink (typexp s t2)
         | _ -> copy_type_desc (typexp s) desc);
    ty'

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  ty'

let label_declaration s l =
  {
    ld_id = l.ld_id;
    ld_mutable = l.ld_mutable;
    ld_optional = l.ld_optional;
    ld_type = typexp s l.ld_type;
    ld_loc = loc s l.ld_loc;
    ld_attributes = attrs s l.ld_attributes;
  }

let constructor_arguments s = function
  | Cstr_tuple l -> Cstr_tuple (List.map (typexp s) l)
  | Cstr_record l -> Cstr_record (List.map (label_declaration s) l)

let constructor_declaration s c =
  {
    cd_id = c.cd_id;
    cd_args = constructor_arguments s c.cd_args;
    cd_res = may_map (typexp s) c.cd_res;
    cd_loc = loc s c.cd_loc;
    cd_attributes = attrs s c.cd_attributes;
  }

let type_declaration s decl =
  let decl =
    {
      type_params = List.map (typexp s) decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        (match decl.type_kind with
        | Type_abstract -> Type_abstract
        | Type_variant cstrs ->
          Type_variant (List.map (constructor_declaration s) cstrs)
        | Type_record (lbls, rep) ->
          Type_record (List.map (label_declaration s) lbls, rep)
        | Type_open -> Type_open);
      type_manifest =
        (match decl.type_manifest with
        | None -> None
        | Some ty -> Some (typexp s ty));
      type_private = decl.type_private;
      type_variance = decl.type_variance;
      type_newtype_level = None;
      type_loc = loc s decl.type_loc;
      type_attributes = attrs s decl.type_attributes;
      type_immediate = decl.type_immediate;
      type_unboxed = decl.type_unboxed;
      type_inlined_types = decl.type_inlined_types;
    }
  in
  cleanup_types ();
  decl

let value_description s descr =
  {
    val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind;
    val_loc = loc s descr.val_loc;
    val_attributes = attrs s descr.val_attributes;
  }

let extension_constructor s ext =
  let ext =
    {
      ext_type_path = type_path s ext.ext_type_path;
      ext_type_params = List.map (typexp s) ext.ext_type_params;
      ext_args = constructor_arguments s ext.ext_args;
      ext_ret_type = may_map (typexp s) ext.ext_ret_type;
      ext_private = ext.ext_private;
      ext_attributes = attrs s ext.ext_attributes;
      ext_loc = (if s.for_saving then Location.none else ext.ext_loc);
      ext_is_exception = ext.ext_is_exception;
    }
  in
  cleanup_types ();
  ext

let rec rename_bound_idents s idents = function
  | [] -> (List.rev idents, s)
  | Sig_type (id, _, _) :: sg ->
    let id' = Ident.rename id in
    rename_bound_idents (add_type id (Pident id') s) (id' :: idents) sg
  | Sig_module (id, _, _) :: sg ->
    let id' = Ident.rename id in
    rename_bound_idents (add_module id (Pident id') s) (id' :: idents) sg
  | Sig_modtype (id, _) :: sg ->
    let id' = Ident.rename id in
    rename_bound_idents
      (add_modtype id (Mty_ident (Pident id')) s)
      (id' :: idents) sg
  | Sig_class_type () :: _ -> assert false
  | (Sig_value (id, _) | Sig_typext (id, _, _)) :: sg ->
    let id' = Ident.rename id in
    rename_bound_idents s (id' :: idents) sg
  | Sig_class _ :: _ -> assert false
let rec modtype s = function
  | Mty_ident p as mty -> (
    match p with
    | Pident id -> ( try Tbl.find id s.modtypes with Not_found -> mty)
    | Pdot (p, n, pos) -> Mty_ident (Pdot (module_path s p, n, pos))
    | Papply _ -> fatal_error "Subst.modtype")
  | Mty_signature sg -> Mty_signature (signature s sg)
  | Mty_functor (id, arg, res) ->
    let id' = Ident.rename id in
    Mty_functor
      (id', may_map (modtype s) arg, modtype (add_module id (Pident id') s) res)
  | Mty_alias (pres, p) -> Mty_alias (pres, module_path s p)

and signature s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let new_idents, s' = rename_bound_idents s [] sg in
  (* ... then apply it to each signature component in turn *)
  List.map2 (signature_component s') sg new_idents

and signature_component s comp newid =
  match comp with
  | Sig_value (_id, d) -> Sig_value (newid, value_description s d)
  | Sig_type (_id, d, rs) -> Sig_type (newid, type_declaration s d, rs)
  | Sig_typext (_id, ext, es) ->
    Sig_typext (newid, extension_constructor s ext, es)
  | Sig_module (_id, d, rs) -> Sig_module (newid, module_declaration s d, rs)
  | Sig_modtype (_id, d) -> Sig_modtype (newid, modtype_declaration s d)
  | Sig_class () -> Sig_class ()
  | Sig_class_type () -> Sig_class_type ()

and module_declaration s decl =
  {
    md_type = modtype s decl.md_type;
    md_attributes = attrs s decl.md_attributes;
    md_loc = loc s decl.md_loc;
  }

and modtype_declaration s decl =
  {
    mtd_type = may_map (modtype s) decl.mtd_type;
    mtd_attributes = attrs s decl.mtd_attributes;
    mtd_loc = loc s decl.mtd_loc;
  }

(* For every binding k |-> d of m1, add k |-> f d to m2
   and return resulting merged map. *)
