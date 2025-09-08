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

(* Operations on module types *)

open Asttypes
open Path
open Types

let rec scrape env mty =
  match mty with
  | Mty_ident p -> (
    try scrape env (Env.find_modtype_expansion p env) with Not_found -> mty)
  | _ -> mty

let freshen mty = Subst.modtype Subst.identity mty

let rec strengthen ~aliasable env mty p =
  match scrape env mty with
  | Mty_signature sg -> Mty_signature (strengthen_sig ~aliasable env sg p 0)
  | Mty_functor (param, arg, res)
    when !Clflags.applicative_functors && Ident.name param <> "*" ->
    Mty_functor
      ( param,
        arg,
        strengthen ~aliasable:false env res (Papply (p, Pident param)) )
  | mty -> mty

and strengthen_sig ~aliasable env sg p pos =
  match sg with
  | [] -> []
  | (Sig_value (_, desc) as sigelt) :: rem ->
    let nextpos =
      match desc.val_kind with
      | Val_prim _ -> pos
      | _ -> pos + 1
    in
    sigelt :: strengthen_sig ~aliasable env rem p nextpos
  | Sig_type (id, {type_kind = Type_abstract}, _)
    :: (Sig_type (id', {type_private = Private}, _) :: _ as rem)
    when Ident.name id = Ident.name id' ^ "#row" ->
    strengthen_sig ~aliasable env rem p pos
  | Sig_type (id, decl, rs) :: rem ->
    let newdecl =
      match (decl.type_manifest, decl.type_private, decl.type_kind) with
      | Some _, Public, _ -> decl
      | Some _, Private, (Type_record _ | Type_variant _) -> decl
      | _ ->
        let manif =
          Some
            (Btype.newgenty
               (Tconstr
                  (Pdot (p, Ident.name id, nopos), decl.type_params, ref Mnil)))
        in
        if decl.type_kind = Type_abstract then
          {decl with type_private = Public; type_manifest = manif}
        else {decl with type_manifest = manif}
    in
    Sig_type (id, newdecl, rs) :: strengthen_sig ~aliasable env rem p pos
  | (Sig_typext _ as sigelt) :: rem ->
    sigelt :: strengthen_sig ~aliasable env rem p (pos + 1)
  | Sig_module (id, md, rs) :: rem ->
    let str =
      strengthen_decl ~aliasable env md (Pdot (p, Ident.name id, pos))
    in
    Sig_module (id, str, rs)
    :: strengthen_sig ~aliasable
         (Env.add_module_declaration ~check:false id md env)
         rem p (pos + 1)
    (* Need to add the module in case it defines manifest module types *)
  | Sig_modtype (id, decl) :: rem ->
    let newdecl =
      match decl.mtd_type with
      | None ->
        {decl with mtd_type = Some (Mty_ident (Pdot (p, Ident.name id, nopos)))}
      | Some _ -> decl
    in
    Sig_modtype (id, newdecl)
    :: strengthen_sig ~aliasable (Env.add_modtype id decl env) rem p pos
    (* Need to add the module type in case it is manifest *)
  | (Sig_class _ as sigelt) :: rem ->
    sigelt :: strengthen_sig ~aliasable env rem p (pos + 1)
  | (Sig_class_type _ as sigelt) :: rem ->
    sigelt :: strengthen_sig ~aliasable env rem p pos

and strengthen_decl ~aliasable env md p =
  match md.md_type with
  | Mty_alias _ -> md
  | _ when aliasable -> {md with md_type = Mty_alias (Mta_present, p)}
  | mty -> {md with md_type = strengthen ~aliasable env mty p}

let () = Env.strengthen := strengthen

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =
  let rec nondep_mty env va mty =
    match mty with
    | Mty_ident p ->
      if Path.isfree mid p then
        nondep_mty env va (Env.find_modtype_expansion p env)
      else mty
    | Mty_alias (_, p) ->
      if Path.isfree mid p then
        nondep_mty env va (Env.find_module p env).md_type
      else mty
    | Mty_signature sg -> Mty_signature (nondep_sig env va sg)
    | Mty_functor (param, arg, res) ->
      let var_inv =
        match va with
        | Co -> Contra
        | Contra -> Co
        | Strict -> Strict
      in
      Mty_functor
        ( param,
          Misc.may_map (nondep_mty env var_inv) arg,
          nondep_mty
            (Env.add_module ~arg:true param (Btype.default_mty arg) env)
            va res )
  and nondep_sig env va = function
    | [] -> []
    | item :: rem -> (
      let rem' = nondep_sig env va rem in
      match item with
      | Sig_value (id, d) ->
        Sig_value (id, {d with val_type = Ctype.nondep_type env mid d.val_type})
        :: rem'
      | Sig_type (id, d, rs) ->
        Sig_type (id, Ctype.nondep_type_decl env mid id (va = Co) d, rs) :: rem'
      | Sig_typext (id, ext, es) ->
        Sig_typext (id, Ctype.nondep_extension_constructor env mid ext, es)
        :: rem'
      | Sig_module (id, md, rs) ->
        Sig_module (id, {md with md_type = nondep_mty env va md.md_type}, rs)
        :: rem'
      | Sig_modtype (id, d) -> (
        try Sig_modtype (id, nondep_modtype_decl env d) :: rem'
        with Not_found -> (
          match va with
          | Co ->
            Sig_modtype
              ( id,
                {mtd_type = None; mtd_loc = Location.none; mtd_attributes = []}
              )
            :: rem'
          | _ -> raise Not_found))
      | Sig_class () -> assert false
      | Sig_class_type () -> assert false)
  and nondep_modtype_decl env mtd =
    {mtd with mtd_type = Misc.may_map (nondep_mty env Strict) mtd.mtd_type}
  in

  nondep_mty env Co mty

let enrich_typedecl env p decl =
  match decl.type_manifest with
  | Some _ -> decl
  | None -> (
    try
      let orig_decl = Env.find_type p env in
      if orig_decl.type_arity <> decl.type_arity then decl
      else
        {
          decl with
          type_manifest =
            Some (Btype.newgenty (Tconstr (p, decl.type_params, ref Mnil)));
        }
    with Not_found -> decl)

let rec enrich_modtype env p mty =
  match mty with
  | Mty_signature sg -> Mty_signature (List.map (enrich_item env p) sg)
  | _ -> mty

and enrich_item env p = function
  | Sig_type (id, decl, rs) ->
    Sig_type (id, enrich_typedecl env (Pdot (p, Ident.name id, nopos)) decl, rs)
  | Sig_module (id, md, rs) ->
    Sig_module
      ( id,
        {
          md with
          md_type =
            enrich_modtype env (Pdot (p, Ident.name id, nopos)) md.md_type;
        },
        rs )
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
  | Mty_ident _ -> []
  | Mty_alias _ -> []
  | Mty_signature sg -> type_paths_sig env p 0 sg
  | Mty_functor _ -> []

and type_paths_sig env p pos sg =
  match sg with
  | [] -> []
  | Sig_value (_id, decl) :: rem ->
    let pos' =
      match decl.val_kind with
      | Val_prim _ -> pos
      | _ -> pos + 1
    in
    type_paths_sig env p pos' rem
  | Sig_type (id, _decl, _) :: rem ->
    Pdot (p, Ident.name id, nopos) :: type_paths_sig env p pos rem
  | Sig_module (id, md, _) :: rem ->
    type_paths env (Pdot (p, Ident.name id, pos)) md.md_type
    @ type_paths_sig
        (Env.add_module_declaration ~check:false id md env)
        p (pos + 1) rem
  | Sig_modtype (id, decl) :: rem ->
    type_paths_sig (Env.add_modtype id decl env) p pos rem
  | (Sig_typext _ | Sig_class _) :: rem -> type_paths_sig env p (pos + 1) rem
  | Sig_class_type _ :: rem -> type_paths_sig env p pos rem

let rec no_code_needed env mty =
  match scrape env mty with
  | Mty_ident _ -> false
  | Mty_signature sg -> no_code_needed_sig env sg
  | Mty_functor (_, _, _) -> false
  | Mty_alias (Mta_absent, _) -> true
  | Mty_alias (Mta_present, _) -> false

and no_code_needed_sig env sg =
  match sg with
  | [] -> true
  | Sig_value (_id, decl) :: rem -> (
    match decl.val_kind with
    | Val_prim _ -> no_code_needed_sig env rem
    | _ -> false)
  | Sig_module (id, md, _) :: rem ->
    no_code_needed env md.md_type
    && no_code_needed_sig
         (Env.add_module_declaration ~check:false id md env)
         rem
  | (Sig_type _ | Sig_modtype _ | Sig_class_type _) :: rem ->
    no_code_needed_sig env rem
  | (Sig_typext _ | Sig_class _) :: _ -> false

(* Check whether a module type may return types *)

let rec contains_type env = function
  | Mty_ident path -> (
    try
      match (Env.find_modtype path env).mtd_type with
      | None -> raise Exit (* PR#6427 *)
      | Some mty -> contains_type env mty
    with Not_found -> raise Exit)
  | Mty_signature sg -> contains_type_sig env sg
  | Mty_functor (_, _, body) -> contains_type env body
  | Mty_alias _ -> ()

and contains_type_sig env = List.iter (contains_type_item env)

and contains_type_item env = function
  | Sig_type
      ( _,
        ( {type_manifest = None}
        | {type_kind = Type_abstract; type_private = Private} ),
        _ )
  | Sig_modtype _
  | Sig_typext (_, {ext_args = Cstr_record _}, _) ->
    (* We consider that extension constructors with an inlined
       record create a type (the inlined record), even though
       it would be technically safe to ignore that considering
       the current constraints which guarantee that this type
       is kept local to expressions. *)
    raise Exit
  | Sig_module (_, {md_type = mty}, _) -> contains_type env mty
  | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_class _ | Sig_class_type _ ->
    ()

let contains_type env mty =
  try
    contains_type env mty;
    false
  with Exit -> true

(* Remove module aliases from a signature *)

module PathSet = Set.Make (Path)
module PathMap = Map.Make (Path)
module IdentSet = Set.Make (Ident)

let rec get_prefixes = function
  | Pident _ -> PathSet.empty
  | Pdot (p, _, _) | Papply (p, _) -> PathSet.add p (get_prefixes p)

let rec get_arg_paths = function
  | Pident _ -> PathSet.empty
  | Pdot (p, _, _) -> get_arg_paths p
  | Papply (p1, p2) ->
    PathSet.add p2
      (PathSet.union (get_prefixes p2)
         (PathSet.union (get_arg_paths p1) (get_arg_paths p2)))

let rec rollback_path subst p =
  try Pident (PathMap.find p subst)
  with Not_found -> (
    match p with
    | Pident _ | Papply _ -> p
    | Pdot (p1, s, n) ->
      let p1' = rollback_path subst p1 in
      if Path.same p1 p1' then p else rollback_path subst (Pdot (p1', s, n)))

let rec collect_ids subst bindings p =
  match rollback_path subst p with
  | Pident id ->
    let ids =
      try collect_ids subst bindings (Ident.find_same id bindings)
      with Not_found -> IdentSet.empty
    in
    IdentSet.add id ids
  | _ -> IdentSet.empty

let collect_arg_paths mty =
  let open Btype in
  let paths = ref PathSet.empty
  and subst = ref PathMap.empty
  and bindings = ref Ident.empty in
  (* let rt = Ident.create "Root" in
     and prefix = ref (Path.Pident rt) in *)
  let it_path p = paths := PathSet.union (get_arg_paths p) !paths
  and it_signature_item it si =
    type_iterators.it_signature_item it si;
    match si with
    | Sig_module (id, {md_type = Mty_alias (_, p)}, _) ->
      bindings := Ident.add id p !bindings
    | Sig_module (id, {md_type = Mty_signature sg}, _) ->
      List.iter
        (function
          | Sig_module (id', _, _) ->
            subst :=
              PathMap.add (Pdot (Pident id, Ident.name id', -1)) id' !subst
          | _ -> ())
        sg
    | _ -> ()
  in
  let it = {type_iterators with it_path; it_signature_item} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty;
  PathSet.fold
    (fun p -> IdentSet.union (collect_ids !subst !bindings p))
    !paths IdentSet.empty

let rec remove_aliases env excl mty =
  match mty with
  | Mty_signature sg -> Mty_signature (remove_aliases_sig env excl sg)
  | Mty_alias _ ->
    let mty' = Env.scrape_alias env mty in
    if mty' = mty then mty
    else (* nested polymorphic comparison *)
      remove_aliases env excl mty'
  | mty -> mty

and remove_aliases_sig env excl sg =
  match sg with
  | [] -> []
  | Sig_module (id, md, rs) :: rem ->
    let mty =
      match md.md_type with
      | Mty_alias _ when IdentSet.mem id excl -> md.md_type
      | mty -> remove_aliases env excl mty
    in
    Sig_module (id, {md with md_type = mty}, rs)
    :: remove_aliases_sig (Env.add_module id mty env) excl rem
  | Sig_modtype (id, mtd) :: rem ->
    Sig_modtype (id, mtd)
    :: remove_aliases_sig (Env.add_modtype id mtd env) excl rem
  | it :: rem -> it :: remove_aliases_sig env excl rem

let remove_aliases env sg =
  let excl = collect_arg_paths sg in
  (* PathSet.iter (fun p -> Format.eprintf "%a@ " Printtyp.path p) excl;
     Format.eprintf "@."; *)
  remove_aliases env excl sg

(* Lower non-generalizable type variables *)

let lower_nongen nglev mty =
  let open Btype in
  let it_type_expr it ty =
    let ty = repr ty in
    match ty with
    | {desc = Tvar _; level} ->
      if level < generic_level && level > nglev then set_level ty nglev
    | _ -> type_iterators.it_type_expr it ty
  in
  let it = {type_iterators with it_type_expr} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty
