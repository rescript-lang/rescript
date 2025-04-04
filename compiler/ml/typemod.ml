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

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Format

type error =
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.error list
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of string * string * Warnings.loc
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

let rescript_hide_attributes (x : Typedtree.attributes) =
  match x with
  | [] -> false
  | ({txt = "internal.local"; _}, _) :: _ -> true
  | _ :: rest -> Ext_list.exists rest (fun (x, _) -> x.txt = "internal.local")

let rescript_hide (x : Typedtree.structure_item_desc) =
  match x with
  | Tstr_module {mb_attributes} -> rescript_hide_attributes mb_attributes
  | _ -> false

open Typedtree

let fst3 (x, _, _) = x

let rec path_concat head p =
  match p with
  | Pident tail -> Pdot (Pident head, Ident.name tail, 0)
  | Pdot (pre, s, pos) -> Pdot (path_concat head pre, s, pos)
  | Papply _ -> assert false

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Env.scrape_alias env mty with
  | Mty_signature sg -> sg
  | Mty_alias (_, path) -> raise (Error (loc, env, Cannot_scrape_alias path))
  | _ -> raise (Error (loc, env, Signature_expected))

let extract_sig_open env loc mty =
  match Env.scrape_alias env mty with
  | Mty_signature sg -> sg
  | Mty_alias (_, path) -> raise (Error (loc, env, Cannot_scrape_alias path))
  | mty -> raise (Error (loc, env, Structure_expected mty))

(* Compute the environment after opening a module *)

let type_open_ ?used_slot ?toplevel ovf env loc lid =
  let path = Typetexp.lookup_module ~load:true env lid.loc lid.txt in
  match Env.open_signature ~loc ?used_slot ?toplevel ovf path env with
  | Some env -> (path, env)
  | None ->
    let md = Env.find_module path env in
    ignore (extract_sig_open env lid.loc md.md_type);
    assert false

let type_open ?toplevel env sod =
  let path, newenv =
    Builtin_attributes.warning_scope sod.popen_attributes (fun () ->
        type_open_ ?toplevel sod.popen_override env sod.popen_loc sod.popen_lid)
  in
  let od =
    {
      open_override = sod.popen_override;
      open_path = path;
      open_txt = sod.popen_lid;
      open_attributes = sod.popen_attributes;
      open_loc = sod.popen_loc;
    }
  in
  (path, newenv, od)

(* Record a module type *)
let rm node =
  Stypes.record (Stypes.Ti_mod node);
  node

(* Forward declaration, to be filled in by type_module_type_of *)
let type_module_type_of_fwd :
    (Env.t ->
    Parsetree.module_expr ->
    Typedtree.module_expr * Types.module_type)
    ref =
  ref (fun _env _m -> assert false)

(* Merge one "with" constraint in a signature *)

let rec add_rec_types env = function
  | Sig_type (id, decl, Trec_next) :: rem ->
    add_rec_types (Env.add_type ~check:true id decl env) rem
  | _ -> env

let check_type_decl env loc id row_id newdecl decl rs rem =
  let env = Env.add_type ~check:true id newdecl env in
  let env =
    match row_id with
    | None -> env
    | Some id -> Env.add_type ~check:false id newdecl env
  in
  let env = if rs = Trec_not then env else add_rec_types env rem in
  Includemod.type_declarations ~loc env id newdecl decl;
  Typedecl.check_coherence env loc id newdecl

let update_rec_next rs rem =
  match rs with
  | Trec_next -> rem
  | Trec_first | Trec_not -> (
    match rem with
    | Sig_type (id, decl, Trec_next) :: rem -> Sig_type (id, decl, rs) :: rem
    | Sig_module (id, mty, Trec_next) :: rem -> Sig_module (id, mty, rs) :: rem
    | _ -> rem)

let make p n i =
  let open Variance in
  set May_pos p (set May_neg n (set May_weak n (set Inj i null)))

let rec iter_path_apply p ~f =
  match p with
  | Pident _ -> ()
  | Pdot (p, _, _) -> iter_path_apply p ~f
  | Papply (p1, p2) ->
    iter_path_apply p1 ~f;
    iter_path_apply p2 ~f;
    f p1 p2 (* after recursing, so we know both paths are well typed *)

let path_is_strict_prefix =
  let rec list_is_strict_prefix l ~prefix =
    match (l, prefix) with
    | [], [] -> false
    | _ :: _, [] -> true
    | [], _ :: _ -> false
    | s1 :: t1, s2 :: t2 ->
      String.equal s1 s2 && list_is_strict_prefix t1 ~prefix:t2
  in
  fun path ~prefix ->
    match (Path.flatten path, Path.flatten prefix) with
    | `Contains_apply, _ | _, `Contains_apply -> false
    | `Ok (ident1, l1), `Ok (ident2, l2) ->
      Ident.same ident1 ident2 && list_is_strict_prefix l1 ~prefix:l2

let iterator_with_env env =
  let env = ref env in
  let super = Btype.type_iterators in
  ( env,
    {
      super with
      Btype.it_signature =
        (fun self sg ->
          (* add all items to the env before recursing down, to handle recursive
             definitions *)
          let env_before = !env in
          List.iter (fun i -> env := Env.add_item i !env) sg;
          super.Btype.it_signature self sg;
          env := env_before);
      Btype.it_module_type =
        (fun self -> function
          | Mty_functor (param, mty_arg, mty_body) ->
            may (self.Btype.it_module_type self) mty_arg;
            let env_before = !env in
            env :=
              Env.add_module ~arg:true param (Btype.default_mty mty_arg) !env;
            self.Btype.it_module_type self mty_body;
            env := env_before
          | mty -> super.Btype.it_module_type self mty);
    } )

let retype_applicative_functor_type ~loc env funct arg =
  let mty_functor = (Env.find_module funct env).md_type in
  let mty_arg = (Env.find_module arg env).md_type in
  let mty_param =
    match Env.scrape_alias env mty_functor with
    | Mty_functor (_, Some mty_param, _) -> mty_param
    | _ -> assert false (* could trigger due to MPR#7611 *)
  in
  let aliasable = not (Env.is_functor_arg arg env) in
  ignore
    (Includemod.modtypes ~loc env
       (Mtype.strengthen ~aliasable env mty_arg arg)
       mty_param)

(* When doing a deep destructive substitution with type M.N.t := .., we change M
   and M.N and so we have to check that uses of the modules other than just
   extracting components from them still make sense. There are only two such
   kinds of uses:
   - applicative functor types: F(M).t might not be well typed anymore
   - aliases: module A = M still makes sense but it doesn't mean the same thing
     anymore, so it's forbidden until it's clear what we should do with it.
   This function would be called with M.N.t and N.t to check for these uses. *)
let check_usage_of_path_of_substituted_item paths env signature ~loc ~lid =
  let iterator =
    let env, super = iterator_with_env env in
    {
      super with
      Btype.it_signature_item =
        (fun self -> function
          | Sig_module (id, {md_type = Mty_alias (_, aliased_path); _}, _)
            when List.exists
                   (fun path -> path_is_strict_prefix path ~prefix:aliased_path)
                   paths ->
            let e = With_changes_module_alias (lid.txt, id, aliased_path) in
            raise (Error (loc, !env, e))
          | sig_item -> super.Btype.it_signature_item self sig_item);
      Btype.it_path =
        (fun referenced_path ->
          iter_path_apply referenced_path ~f:(fun funct arg ->
              if
                List.exists
                  (fun path -> path_is_strict_prefix path ~prefix:arg)
                  paths
              then
                let env = !env in
                try retype_applicative_functor_type ~loc env funct arg
                with Includemod.Error explanation ->
                  raise
                    (Error
                       ( loc,
                         env,
                         With_makes_applicative_functor_ill_typed
                           (lid.txt, referenced_path, explanation) ))));
    }
  in
  iterator.Btype.it_signature iterator signature;
  Btype.unmark_iterators.Btype.it_signature Btype.unmark_iterators signature

let type_decl_is_alias sdecl =
  (* assuming no explicit constraint *)
  match sdecl.ptype_manifest with
  | Some {ptyp_desc = Ptyp_constr (lid, stl)}
    when List.length stl = List.length sdecl.ptype_params -> (
    match
      List.iter2
        (fun x (y, _) ->
          match (x, y) with
          | {ptyp_desc = Ptyp_var sx}, {ptyp_desc = Ptyp_var sy} when sx = sy ->
            ()
          | _, _ -> raise Exit)
        stl sdecl.ptype_params
    with
    | exception Exit -> None
    | () -> Some lid)
  | _ -> None

let params_are_constrained =
  let rec loop = function
    | [] -> false
    | hd :: tl -> (
      match (Btype.repr hd).desc with
      | Tvar _ -> List.memq hd tl || loop tl
      | _ -> true)
  in
  loop

let merge_constraint initial_env loc sg constr =
  let lid =
    match constr with
    | Pwith_type (lid, _)
    | Pwith_module (lid, _)
    | Pwith_typesubst (lid, _)
    | Pwith_modsubst (lid, _) ->
      lid
  in
  let destructive_substitution =
    match constr with
    | Pwith_type _ | Pwith_module _ -> false
    | Pwith_typesubst _ | Pwith_modsubst _ -> true
  in
  let real_ids = ref [] in
  let rec merge env sg namelist row_id =
    match (sg, namelist, constr) with
    | [], _, _ -> raise (Error (loc, env, With_no_component lid.txt))
    | ( Sig_type (id, decl, rs) :: rem,
        [s],
        Pwith_type (_, ({ptype_kind = Ptype_abstract} as sdecl)) )
      when Ident.name id = s && Typedecl.is_fixed_type sdecl ->
      let decl_row =
        {
          type_params = List.map (fun _ -> Btype.newgenvar ()) sdecl.ptype_params;
          type_arity = List.length sdecl.ptype_params;
          type_kind = Type_abstract;
          type_private = Private;
          type_manifest = None;
          type_variance =
            List.map
              (fun (_, v) ->
                let c, n =
                  match v with
                  | Covariant -> (true, false)
                  | Contravariant -> (false, true)
                  | Invariant -> (false, false)
                in
                make (not n) (not c) false)
              sdecl.ptype_params;
          type_loc = sdecl.ptype_loc;
          type_newtype_level = None;
          type_attributes = [];
          type_immediate = false;
          type_unboxed = unboxed_false_default_false;
          type_inlined_types = [];
        }
      and id_row = Ident.create (s ^ "#row") in
      let initial_env = Env.add_type ~check:false id_row decl_row initial_env in
      let tdecl =
        Typedecl.transl_with_constraint initial_env id (Some (Pident id_row))
          decl sdecl
      in
      let newdecl = tdecl.typ_type in
      check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
      let decl_row = {decl_row with type_params = newdecl.type_params} in
      let rs' = if rs = Trec_first then Trec_not else rs in
      ( (Pident id, lid, Twith_type tdecl),
        Sig_type (id_row, decl_row, rs') :: Sig_type (id, newdecl, rs) :: rem )
    | Sig_type (id, decl, rs) :: rem, [s], Pwith_type (_, sdecl)
      when Ident.name id = s ->
      let tdecl =
        Typedecl.transl_with_constraint initial_env id None decl sdecl
      in
      let newdecl = tdecl.typ_type in
      check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
      ((Pident id, lid, Twith_type tdecl), Sig_type (id, newdecl, rs) :: rem)
    | Sig_type (id, _, _) :: rem, [s], (Pwith_type _ | Pwith_typesubst _)
      when Ident.name id = s ^ "#row" ->
      merge env rem namelist (Some id)
    | Sig_type (id, decl, rs) :: rem, [s], Pwith_typesubst (_, sdecl)
      when Ident.name id = s ->
      (* Check as for a normal with constraint, but discard definition *)
      let tdecl =
        Typedecl.transl_with_constraint initial_env id None decl sdecl
      in
      let newdecl = tdecl.typ_type in
      check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
      real_ids := [Pident id];
      ((Pident id, lid, Twith_typesubst tdecl), update_rec_next rs rem)
    | Sig_module (id, md, rs) :: rem, [s], Pwith_module (_, lid')
      when Ident.name id = s ->
      let path, md' = Typetexp.find_module initial_env loc lid'.txt in
      let md'' = {md' with md_type = Mtype.remove_aliases env md'.md_type} in
      let newmd = Mtype.strengthen_decl ~aliasable:false env md'' path in
      ignore (Includemod.modtypes ~loc env newmd.md_type md.md_type);
      ( (Pident id, lid, Twith_module (path, lid')),
        Sig_module (id, newmd, rs) :: rem )
    | Sig_module (id, md, rs) :: rem, [s], Pwith_modsubst (_, lid')
      when Ident.name id = s ->
      let path, md' = Typetexp.find_module initial_env loc lid'.txt in
      let newmd = Mtype.strengthen_decl ~aliasable:false env md' path in
      ignore (Includemod.modtypes ~loc env newmd.md_type md.md_type);
      real_ids := [Pident id];
      ((Pident id, lid, Twith_modsubst (path, lid')), update_rec_next rs rem)
    | Sig_module (id, md, rs) :: rem, s :: namelist, _ when Ident.name id = s ->
      let (path, _path_loc, tcstr), newsg =
        merge env (extract_sig env loc md.md_type) namelist None
      in
      let path = path_concat id path in
      real_ids := path :: !real_ids;
      let item = Sig_module (id, {md with md_type = Mty_signature newsg}, rs) in
      ((path, lid, tcstr), item :: rem)
    | item :: rem, _, _ ->
      let cstr, items = merge (Env.add_item item env) rem namelist row_id in
      (cstr, item :: items)
  in
  try
    let names = Longident.flatten lid.txt in
    let tcstr, sg = merge initial_env sg names None in
    (if destructive_substitution then
       match List.rev !real_ids with
       | [] -> assert false
       | last :: rest -> (
         (* The last item is the one that's removed. We don't need to check how
            it's used since it's replaced by a more specific type/module. *)
         assert (
           match last with
           | Pident _ -> true
           | _ -> false);
         match rest with
         | [] -> ()
         | _ :: _ ->
           check_usage_of_path_of_substituted_item rest initial_env sg ~loc ~lid
         ));
    let sg =
      match tcstr with
      | _, _, Twith_typesubst tdecl ->
        let how_to_extend_subst =
          let sdecl =
            match constr with
            | Pwith_typesubst (_, sdecl) -> sdecl
            | _ -> assert false
          in
          match type_decl_is_alias sdecl with
          | Some lid ->
            let replacement =
              try Env.lookup_type lid.txt initial_env
              with Not_found -> assert false
            in
            fun s path -> Subst.add_type_path path replacement s
          | None ->
            let body =
              match tdecl.typ_type.type_manifest with
              | None -> assert false
              | Some x -> x
            in
            let params = tdecl.typ_type.type_params in
            if params_are_constrained params then
              raise
                (Error (loc, initial_env, With_cannot_remove_constrained_type));
            fun s path -> Subst.add_type_function path ~params ~body s
        in
        let sub = List.fold_left how_to_extend_subst Subst.identity !real_ids in
        Subst.signature sub sg
      | _, _, Twith_modsubst (real_path, _) ->
        let sub =
          List.fold_left
            (fun s path -> Subst.add_module_path path real_path s)
            Subst.identity !real_ids
        in
        Subst.signature sub sg
      | _ -> sg
    in
    (tcstr, sg)
  with Includemod.Error explanation ->
    raise (Error (loc, initial_env, With_mismatch (lid.txt, explanation)))

(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn Trec_first d1 :: map_end (fn Trec_next) dl rem

let map_rec_type ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
    let first =
      match rec_flag with
      | Recursive -> Trec_first
      | Nonrecursive -> Trec_not
    in
    fn first d1 :: map_end (fn Trec_next) dl rem

let rec map_rec_type_with_row_types ~rec_flag fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl ->
    if Btype.is_row_name (Ident.name d1.typ_id) then
      fn Trec_not d1 :: map_rec_type_with_row_types ~rec_flag fn dl rem
    else map_rec_type ~rec_flag fn decls rem

(* Add type extension flags to extension constructors *)
let map_ext fn exts rem =
  match exts with
  | [] -> rem
  | d1 :: dl -> fn Text_first d1 :: map_end (fn Text_next) dl rem

(* Auxiliary for translating recursively-defined module types.
   Return a module type that approximates the shape of the given module
   type AST.  Retain only module, type, and module type
   components of signatures.  For types, retain only their arity,
   making them abstract otherwise. *)

let rec approx_modtype env smty =
  match smty.pmty_desc with
  | Pmty_ident lid ->
    let path, _info = Typetexp.find_modtype env smty.pmty_loc lid.txt in
    Mty_ident path
  | Pmty_alias lid ->
    let path = Typetexp.lookup_module env smty.pmty_loc lid.txt in
    Mty_alias (Mta_absent, path)
  | Pmty_signature ssg -> Mty_signature (approx_sig env ssg)
  | Pmty_functor (param, sarg, sres) ->
    let arg = may_map (approx_modtype env) sarg in
    let id, newenv =
      Env.enter_module ~arg:true param.txt (Btype.default_mty arg) env
    in
    let res = approx_modtype newenv sres in
    Mty_functor (id, arg, res)
  | Pmty_with (sbody, _constraints) -> approx_modtype env sbody
  | Pmty_typeof smod ->
    let _, mty = !type_module_type_of_fwd env smod in
    mty
  | Pmty_extension ext ->
    raise (Error_forward (Builtin_attributes.error_of_extension ext))

and approx_module_declaration env pmd =
  {
    Types.md_type = approx_modtype env pmd.pmd_type;
    md_attributes = pmd.pmd_attributes;
    md_loc = pmd.pmd_loc;
  }

and approx_sig env ssg =
  match ssg with
  | [] -> []
  | item :: srem -> (
    match item.psig_desc with
    | Psig_type (rec_flag, sdecls) ->
      let decls = Typedecl.approx_type_decl sdecls in
      let rem = approx_sig env srem in
      map_rec_type ~rec_flag
        (fun rs (id, info) -> Sig_type (id, info, rs))
        decls rem
    | Psig_module pmd ->
      let id = Ident.create pmd.pmd_name.txt in
      let md = approx_module_declaration env pmd in
      let newenv = Env.enter_module_declaration id md env in
      Sig_module (id, md, Trec_not) :: approx_sig newenv srem
    | Psig_recmodule sdecls ->
      let decls =
        List.map
          (fun pmd ->
            (Ident.create pmd.pmd_name.txt, approx_module_declaration env pmd))
          sdecls
      in
      let newenv =
        List.fold_left
          (fun env (id, md) ->
            Env.add_module_declaration ~check:false id md env)
          env decls
      in
      map_rec
        (fun rs (id, md) -> Sig_module (id, md, rs))
        decls (approx_sig newenv srem)
    | Psig_modtype d ->
      let info = approx_modtype_info env d in
      let id, newenv = Env.enter_modtype d.pmtd_name.txt info env in
      Sig_modtype (id, info) :: approx_sig newenv srem
    | Psig_open sod ->
      let _path, mty, _od = type_open env sod in
      approx_sig mty srem
    | Psig_include sincl ->
      let smty = sincl.pincl_mod in
      let mty = approx_modtype env smty in
      let sg =
        Subst.signature Subst.identity (extract_sig env smty.pmty_loc mty)
      in
      let newenv = Env.add_signature sg env in
      sg @ approx_sig newenv srem
    | _ -> approx_sig env srem)

and approx_modtype_info env sinfo =
  {
    mtd_type = may_map (approx_modtype env) sinfo.pmtd_type;
    mtd_attributes = sinfo.pmtd_attributes;
    mtd_loc = sinfo.pmtd_loc;
  }

let approx_modtype env smty =
  Warnings.without_warnings (fun () -> approx_modtype env smty)

(* Additional validity checks on type definitions arising from
   recursive modules *)

let check_recmod_typedecls env sdecls decls =
  let recmod_ids = List.map fst3 decls in
  List.iter2
    (fun pmd (id, _, mty) ->
      let mty = mty.mty_type in
      List.iter
        (fun path ->
          Typedecl.check_recmod_typedecl env pmd.pmd_type.pmty_loc recmod_ids
            path (Env.find_type path env))
        (Mtype.type_paths env (Pident id) mty))
    sdecls decls

(* Auxiliaries for checking uniqueness of names in signatures and structures *)

module StringSet = Set.Make (struct
  type t = string
  let compare (x : t) y = String.compare x y
end)

let check cl loc tbl name =
  match Hashtbl.find_opt tbl name with
  | Some repeated_loc ->
    raise (Error (loc, Env.empty, Repeated_name (cl, name, repeated_loc)))
  | None -> Hashtbl.add tbl name loc

type names = {
  types: (string, Warnings.loc) Hashtbl.t;
  modules: (string, Warnings.loc) Hashtbl.t;
  modtypes: (string, Warnings.loc) Hashtbl.t;
  typexts: (string, Warnings.loc) Hashtbl.t;
}

let new_names () =
  {
    types = Hashtbl.create 10;
    modules = Hashtbl.create 10;
    modtypes = Hashtbl.create 10;
    typexts = Hashtbl.create 10;
  }

let check_name check names name = check names name.loc name.txt
let check_type names loc s = check "type" loc names.types s
let check_module names loc s = check "module" loc names.modules s
let check_modtype names loc s = check "module type" loc names.modtypes s
let check_typext names loc s = check "extension constructor" loc names.typexts s

let check_sig_item names loc = function
  | Sig_type (id, _, _) -> check_type names loc (Ident.name id)
  | Sig_module (id, _, _) -> check_module names loc (Ident.name id)
  | Sig_modtype (id, _) -> check_modtype names loc (Ident.name id)
  | Sig_typext (id, _, _) -> check_typext names loc (Ident.name id)
  | _ -> ()

(* Simplify multiple specifications of a value or an extension in a signature.
   (Other signature components, e.g. types, modules, etc, are checked for
   name uniqueness.)  If multiple specifications with the same name,
   keep only the last (rightmost) one. *)

let simplify_signature sg =
  let rec aux = function
    | [] -> ([], StringSet.empty)
    | (Sig_value (id, _descr) as component) :: sg ->
      let ((sg, val_names) as k) = aux sg in
      let name = Ident.name id in
      if StringSet.mem name val_names then k
      else (component :: sg, StringSet.add name val_names)
    | component :: sg ->
      let sg, val_names = aux sg in
      (component :: sg, val_names)
  in
  let sg, _ = aux sg in
  sg

(* Check and translate a module type expression *)

let transl_modtype_longident loc env lid =
  let path, _info = Typetexp.find_modtype env loc lid in
  path

let transl_module_alias loc env lid = Typetexp.lookup_module env loc lid

let mkmty desc typ env loc attrs =
  let mty =
    {
      mty_desc = desc;
      mty_type = typ;
      mty_loc = loc;
      mty_env = env;
      mty_attributes = attrs;
    }
  in
  Cmt_format.add_saved_type (Cmt_format.Partial_module_type mty);
  mty

let mksig desc env loc =
  let sg = {sig_desc = desc; sig_loc = loc; sig_env = env} in
  Cmt_format.add_saved_type (Cmt_format.Partial_signature_item sg);
  sg

(* let signature sg = List.map (fun item -> item.sig_type) sg *)

let rec transl_modtype env smty =
  Builtin_attributes.warning_scope smty.pmty_attributes (fun () ->
      transl_modtype_aux env smty)

and transl_modtype_aux env smty =
  let loc = smty.pmty_loc in
  match smty.pmty_desc with
  | Pmty_ident lid ->
    let path = transl_modtype_longident loc env lid.txt in
    mkmty (Tmty_ident (path, lid)) (Mty_ident path) env loc smty.pmty_attributes
  | Pmty_alias lid ->
    let path = transl_module_alias loc env lid.txt in
    mkmty
      (Tmty_alias (path, lid))
      (Mty_alias (Mta_absent, path))
      env loc smty.pmty_attributes
  | Pmty_signature ssg ->
    let sg = transl_signature env ssg in
    mkmty (Tmty_signature sg) (Mty_signature sg.sig_type) env loc
      smty.pmty_attributes
  | Pmty_functor (param, sarg, sres) ->
    let arg = Misc.may_map (transl_modtype env) sarg in
    let ty_arg = Misc.may_map (fun m -> m.mty_type) arg in
    let id, newenv =
      Env.enter_module ~arg:true param.txt (Btype.default_mty ty_arg) env
    in
    Ctype.init_def (Ident.current_time ());
    (* PR#6513 *)
    let res = transl_modtype newenv sres in
    mkmty
      (Tmty_functor (id, param, arg, res))
      (Mty_functor (id, ty_arg, res.mty_type))
      env loc smty.pmty_attributes
  | Pmty_with (sbody, constraints) ->
    let body = transl_modtype env sbody in
    let init_sg = extract_sig env sbody.pmty_loc body.mty_type in
    let rev_tcstrs, final_sg =
      List.fold_left
        (fun (rev_tcstrs, sg) sdecl ->
          let tcstr, sg = merge_constraint env smty.pmty_loc sg sdecl in
          (tcstr :: rev_tcstrs, sg))
        ([], init_sg) constraints
    in
    mkmty
      (Tmty_with (body, List.rev rev_tcstrs))
      (Mtype.freshen (Mty_signature final_sg))
      env loc smty.pmty_attributes
  | Pmty_typeof smod ->
    let env = Env.in_signature false env in
    let tmty, mty = !type_module_type_of_fwd env smod in
    mkmty (Tmty_typeof tmty) mty env loc smty.pmty_attributes
  | Pmty_extension ext ->
    raise (Error_forward (Builtin_attributes.error_of_extension ext))

and transl_signature env sg =
  let names = new_names () in
  let rec transl_sig env sg =
    Ctype.init_def (Ident.current_time ());
    match sg with
    | [] -> ([], [], env)
    | item :: srem -> (
      let loc = item.psig_loc in
      match item.psig_desc with
      | Psig_value sdesc ->
        let tdesc, newenv =
          Typedecl.transl_value_decl env item.psig_loc sdesc
        in
        let trem, rem, final_env = transl_sig newenv srem in
        ( mksig (Tsig_value tdesc) env loc :: trem,
          Sig_value (tdesc.val_id, tdesc.val_val) :: rem,
          final_env )
      | Psig_type (rec_flag, sdecls) ->
        List.iter
          (fun decl -> check_name check_type names decl.ptype_name)
          sdecls;
        let decls, newenv = Typedecl.transl_type_decl env rec_flag sdecls in
        let trem, rem, final_env = transl_sig newenv srem in
        ( mksig (Tsig_type (rec_flag, decls)) env loc :: trem,
          map_rec_type_with_row_types ~rec_flag
            (fun rs td -> Sig_type (td.typ_id, td.typ_type, rs))
            decls rem,
          final_env )
      | Psig_typext styext ->
        List.iter
          (fun pext -> check_name check_typext names pext.pext_name)
          styext.ptyext_constructors;
        let tyext, newenv =
          Typedecl.transl_type_extension false env item.psig_loc styext
        in
        let trem, rem, final_env = transl_sig newenv srem in
        let constructors = tyext.tyext_constructors in
        ( mksig (Tsig_typext tyext) env loc :: trem,
          map_ext
            (fun es ext -> Sig_typext (ext.ext_id, ext.ext_type, es))
            constructors rem,
          final_env )
      | Psig_exception sext ->
        check_name check_typext names sext.pext_name;
        let ext, newenv = Typedecl.transl_exception env sext in
        let trem, rem, final_env = transl_sig newenv srem in
        ( mksig (Tsig_exception ext) env loc :: trem,
          Sig_typext (ext.ext_id, ext.ext_type, Text_exception) :: rem,
          final_env )
      | Psig_module pmd ->
        check_name check_module names pmd.pmd_name;
        let id = Ident.create pmd.pmd_name.txt in
        let tmty =
          Builtin_attributes.warning_scope pmd.pmd_attributes (fun () ->
              transl_modtype env pmd.pmd_type)
        in
        let md =
          {
            md_type = tmty.mty_type;
            md_attributes = pmd.pmd_attributes;
            md_loc = pmd.pmd_loc;
          }
        in
        let newenv = Env.enter_module_declaration id md env in
        let trem, rem, final_env = transl_sig newenv srem in
        ( mksig
            (Tsig_module
               {
                 md_id = id;
                 md_name = pmd.pmd_name;
                 md_type = tmty;
                 md_loc = pmd.pmd_loc;
                 md_attributes = pmd.pmd_attributes;
               })
            env loc
          :: trem,
          Sig_module (id, md, Trec_not) :: rem,
          final_env )
      | Psig_recmodule sdecls ->
        List.iter (fun pmd -> check_name check_module names pmd.pmd_name) sdecls;
        let decls, newenv = transl_recmodule_modtypes env sdecls in
        let trem, rem, final_env = transl_sig newenv srem in
        ( mksig (Tsig_recmodule decls) env loc :: trem,
          map_rec
            (fun rs md ->
              let d =
                {
                  Types.md_type = md.md_type.mty_type;
                  md_attributes = md.md_attributes;
                  md_loc = md.md_loc;
                }
              in
              Sig_module (md.md_id, d, rs))
            decls rem,
          final_env )
      | Psig_modtype pmtd ->
        let newenv, mtd, sg = transl_modtype_decl names env pmtd in
        let trem, rem, final_env = transl_sig newenv srem in
        (mksig (Tsig_modtype mtd) env loc :: trem, sg :: rem, final_env)
      | Psig_open sod ->
        let _path, newenv, od = type_open env sod in
        let trem, rem, final_env = transl_sig newenv srem in
        (mksig (Tsig_open od) env loc :: trem, rem, final_env)
      | Psig_include sincl ->
        let smty = sincl.pincl_mod in
        let tmty =
          Builtin_attributes.warning_scope sincl.pincl_attributes (fun () ->
              transl_modtype env smty)
        in
        let mty = tmty.mty_type in
        let sg =
          Subst.signature Subst.identity (extract_sig env smty.pmty_loc mty)
        in
        List.iter (check_sig_item names item.psig_loc) sg;
        let newenv = Env.add_signature sg env in
        let incl =
          {
            incl_mod = tmty;
            incl_type = sg;
            incl_attributes = sincl.pincl_attributes;
            incl_loc = sincl.pincl_loc;
          }
        in
        let trem, rem, final_env = transl_sig newenv srem in
        (mksig (Tsig_include incl) env loc :: trem, sg @ rem, final_env)
      | Psig_attribute x ->
        Builtin_attributes.warning_attribute x;
        let trem, rem, final_env = transl_sig env srem in
        (mksig (Tsig_attribute x) env loc :: trem, rem, final_env)
      | Psig_extension (ext, _attrs) ->
        raise (Error_forward (Builtin_attributes.error_of_extension ext)))
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  Builtin_attributes.warning_scope [] (fun () ->
      let trem, rem, final_env = transl_sig (Env.in_signature true env) sg in
      let rem = simplify_signature rem in
      let sg = {sig_items = trem; sig_type = rem; sig_final_env = final_env} in
      Cmt_format.set_saved_types
        (Cmt_format.Partial_signature sg :: previous_saved_types);
      sg)

and transl_modtype_decl names env pmtd =
  Builtin_attributes.warning_scope pmtd.pmtd_attributes (fun () ->
      transl_modtype_decl_aux names env pmtd)

and transl_modtype_decl_aux names env
    {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} =
  check_name check_modtype names pmtd_name;
  let tmty = Misc.may_map (transl_modtype env) pmtd_type in
  let decl =
    {
      Types.mtd_type = may_map (fun t -> t.mty_type) tmty;
      mtd_attributes = pmtd_attributes;
      mtd_loc = pmtd_loc;
    }
  in
  let id, newenv = Env.enter_modtype pmtd_name.txt decl env in
  let mtd =
    {
      mtd_id = id;
      mtd_name = pmtd_name;
      mtd_type = tmty;
      mtd_attributes = pmtd_attributes;
      mtd_loc = pmtd_loc;
    }
  in
  (newenv, mtd, Sig_modtype (id, decl))

and transl_recmodule_modtypes env sdecls =
  let make_env curr =
    List.fold_left
      (fun env (id, _, mty) -> Env.add_module ~arg:true id mty env)
      env curr
  in
  let make_env2 curr =
    List.fold_left
      (fun env (id, _, mty) -> Env.add_module ~arg:true id mty.mty_type env)
      env curr
  in
  let transition env_c curr =
    List.map2
      (fun pmd (id, id_loc, _mty) ->
        let tmty =
          Builtin_attributes.warning_scope pmd.pmd_attributes (fun () ->
              transl_modtype env_c pmd.pmd_type)
        in
        (id, id_loc, tmty))
      sdecls curr
  in
  let ids = List.map (fun x -> Ident.create x.pmd_name.txt) sdecls in
  let approx_env =
    (*
       cf #5965
       We use a dummy module type in order to detect a reference to one
       of the module being defined during the call to approx_modtype.
       It will be detected in Env.lookup_module.
    *)
    List.fold_left
      (fun env id ->
        let dummy = Mty_ident (Path.Pident (Ident.create "#recmod#")) in
        Env.add_module ~arg:true id dummy env)
      env ids
  in
  Ctype.init_def (Ident.current_time ());
  (* PR#7082 *)
  let init =
    List.map2
      (fun id pmd -> (id, pmd.pmd_name, approx_modtype approx_env pmd.pmd_type))
      ids sdecls
  in
  let env0 = make_env init in
  let dcl1 = Warnings.without_warnings (fun () -> transition env0 init) in
  let env1 = make_env2 dcl1 in
  check_recmod_typedecls env1 sdecls dcl1;
  let dcl2 = transition env1 dcl1 in
  (*
  List.iter
    (fun (id, mty) ->
      Format.printf "%a: %a@." Printtyp.ident id Printtyp.modtype mty)
    dcl2;
*)
  let env2 = make_env2 dcl2 in
  check_recmod_typedecls env2 sdecls dcl2;
  let dcl2 =
    List.map2
      (fun pmd (id, id_loc, mty) ->
        {
          md_id = id;
          md_name = id_loc;
          md_type = mty;
          md_loc = pmd.pmd_loc;
          md_attributes = pmd.pmd_attributes;
        })
      sdecls dcl2
  in
  (dcl2, env2)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
  | Tmod_ident (p, _) -> p
  | Tmod_apply (funct, arg, _coercion) when !Clflags.applicative_functors ->
    Papply (path_of_module funct, path_of_module arg)
  | Tmod_constraint (mexp, _, _, _) -> path_of_module mexp
  | _ -> raise Not_a_path

let path_of_module mexp =
  try Some (path_of_module mexp) with Not_a_path -> None

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype env = function
  | Mty_ident _ -> true
  | Mty_alias _ -> true
  | Mty_signature sg ->
    let env = Env.add_signature sg env in
    List.for_all (closed_signature_item env) sg
  | Mty_functor (id, param, body) ->
    let env = Env.add_module ~arg:true id (Btype.default_mty param) env in
    closed_modtype env body

and closed_signature_item env = function
  | Sig_value (_id, desc) -> Ctype.closed_schema env desc.val_type
  | Sig_module (_id, md, _) -> closed_modtype env md.md_type
  | _ -> true

let check_nongen_scheme env sig_item =
  match sig_item with
  | Sig_value (_id, vd) ->
    if not (Ctype.closed_schema env vd.val_type) then
      raise (Error (vd.val_loc, env, Non_generalizable vd.val_type))
  | Sig_module (_id, md, _) ->
    if not (closed_modtype env md.md_type) then
      raise (Error (md.md_loc, env, Non_generalizable_module md.md_type))
  | _ -> ()

let check_nongen_schemes env sg = List.iter (check_nongen_scheme env) sg

(* Helpers for typing recursive modules *)

let anchor_submodule name anchor =
  match anchor with
  | None -> None
  | Some p -> Some (Pdot (p, name, nopos))
let anchor_recmodule id = Some (Pident id)

let enrich_type_decls anchor decls oldenv newenv =
  match anchor with
  | None -> newenv
  | Some p ->
    List.fold_left
      (fun e info ->
        let id = info.typ_id in
        let info' =
          Mtype.enrich_typedecl oldenv
            (Pdot (p, Ident.name id, nopos))
            info.typ_type
        in
        Env.add_type ~check:true id info' e)
      oldenv decls

let enrich_module_type anchor name mty env =
  match anchor with
  | None -> mty
  | Some p -> Mtype.enrich_modtype env (Pdot (p, name, nopos)) mty

let check_recmodule_inclusion env bindings =
  (* PR#4450, PR#4470: consider
        module rec X : DECL = MOD  where MOD has inferred type ACTUAL
     The "natural" typing condition
        E, X: ACTUAL |- ACTUAL <: DECL
     leads to circularities through manifest types.
     Instead, we "unroll away" the potential circularities a finite number
     of times.  The (weaker) condition we implement is:
        E, X: DECL,
           X1: ACTUAL,
           X2: ACTUAL{X <- X1}/X1
           ...
           Xn: ACTUAL{X <- X(n-1)}/X(n-1)
        |- ACTUAL{X <- Xn}/Xn <: DECL{X <- Xn}
     so that manifest types rooted at X(n+1) are expanded in terms of X(n),
     avoiding circularities.  The strengthenings ensure that
     Xn.t = X(n-1).t = ... = X2.t = X1.t.
     N can be chosen arbitrarily; larger values of N result in more
     recursive definitions being accepted.  A good choice appears to be
     the number of mutually recursive declarations. *)
  let subst_and_strengthen env s id mty =
    Mtype.strengthen ~aliasable:false env (Subst.modtype s mty)
      (Subst.module_path s (Pident id))
  in

  let rec check_incl first_time n env s =
    if n > 0 then
      (* Generate fresh names Y_i for the rec. bound module idents X_i *)
      let bindings1 =
        List.map
          (fun (id, _, _mty_decl, _modl, mty_actual, _attrs, _loc) ->
            (id, Ident.rename id, mty_actual))
          bindings
      in
      (* Enter the Y_i in the environment with their actual types substituted
         by the input substitution s *)
      let env' =
        List.fold_left
          (fun env (id, id', mty_actual) ->
            let mty_actual' =
              if first_time then mty_actual
              else subst_and_strengthen env s id mty_actual
            in
            Env.add_module ~arg:false id' mty_actual' env)
          env bindings1
      in
      (* Build the output substitution Y_i <- X_i *)
      let s' =
        List.fold_left
          (fun s (id, id', _mty_actual) -> Subst.add_module id (Pident id') s)
          Subst.identity bindings1
      in
      (* Recurse with env' and s' *)
      check_incl false (n - 1) env' s'
    else
      (* Base case: check inclusion of s(mty_actual) in s(mty_decl)
         and insert coercion if needed *)
      let check_inclusion (id, id_loc, mty_decl, modl, mty_actual, attrs, loc) =
        let mty_decl' = Subst.modtype s mty_decl.mty_type
        and mty_actual' = subst_and_strengthen env s id mty_actual in
        let coercion =
          try Includemod.modtypes ~loc:modl.mod_loc env mty_actual' mty_decl'
          with Includemod.Error msg ->
            raise (Error (modl.mod_loc, env, Not_included msg))
        in
        let modl' =
          {
            mod_desc =
              Tmod_constraint
                (modl, mty_decl.mty_type, Tmodtype_explicit mty_decl, coercion);
            mod_type = mty_decl.mty_type;
            mod_env = env;
            mod_loc = modl.mod_loc;
            mod_attributes = [];
          }
        in
        {
          mb_id = id;
          mb_name = id_loc;
          mb_expr = modl';
          mb_attributes = attrs;
          mb_loc = loc;
        }
      in
      List.map check_inclusion bindings
  in
  check_incl true (List.length bindings) env Subst.identity

(* Helper for unpack *)

let rec package_constraints env loc mty constrs =
  if constrs = [] then mty
  else
    let sg = extract_sig env loc mty in
    let sg' =
      List.map
        (function
          | Sig_type (id, ({type_params = []} as td), rs)
            when List.mem_assoc [Ident.name id] constrs ->
            let ty = List.assoc [Ident.name id] constrs in
            Sig_type (id, {td with type_manifest = Some ty}, rs)
          | Sig_module (id, md, rs) ->
            let rec aux = function
              | (m :: (_ :: _ as l), t) :: rest when m = Ident.name id ->
                (l, t) :: aux rest
              | _ :: rest -> aux rest
              | [] -> []
            in
            let md =
              {
                md with
                md_type = package_constraints env loc md.md_type (aux constrs);
              }
            in
            Sig_module (id, md, rs)
          | item -> item)
        sg
    in
    Mty_signature sg'

let modtype_of_package env loc p nl tl =
  try
    match (Env.find_modtype p env).mtd_type with
    | Some mty when nl <> [] ->
      package_constraints env loc mty
        (List.combine (List.map Longident.flatten nl) tl)
    | _ ->
      if nl = [] then Mty_ident p
      else raise (Error (loc, env, Signature_expected))
  with Not_found ->
    let error = Typetexp.Unbound_modtype (Ctype.lid_of_path p) in
    raise (Typetexp.Error (loc, env, error))

let package_subtype env p1 nl1 tl1 p2 nl2 tl2 =
  let mkmty p nl tl =
    let ntl =
      Ext_list.filter (List.combine nl tl) (fun (_n, t) ->
          Ctype.free_variables t = [])
    in
    let nl, tl = List.split ntl in
    modtype_of_package env Location.none p nl tl
  in
  let mty1 = mkmty p1 nl1 tl1 and mty2 = mkmty p2 nl2 tl2 in
  try Includemod.modtypes ~loc:Location.none env mty1 mty2 = Tcoerce_none
  with Includemod.Error _msg -> false
(* raise(Error(Location.none, env, Not_included msg)) *)

let () = Ctype.package_subtype := package_subtype

let wrap_constraint env arg mty explicit =
  let coercion =
    try Includemod.modtypes ~loc:arg.mod_loc env arg.mod_type mty
    with Includemod.Error msg ->
      raise (Error (arg.mod_loc, env, Not_included msg))
  in
  {
    mod_desc = Tmod_constraint (arg, mty, explicit, coercion);
    mod_type = mty;
    mod_env = env;
    mod_attributes = [];
    mod_loc = arg.mod_loc;
  }

(* Type a module value expression *)

let rec type_module ?(alias = false) sttn funct_body anchor env smod =
  Builtin_attributes.warning_scope smod.pmod_attributes (fun () ->
      type_module_aux ~alias sttn funct_body anchor env smod)

and type_module_aux ~alias sttn funct_body anchor env smod =
  match smod.pmod_desc with
  | Pmod_ident lid ->
    let path =
      Typetexp.lookup_module ~load:(not alias) env smod.pmod_loc lid.txt
    in
    let md =
      {
        mod_desc = Tmod_ident (path, lid);
        mod_type = Mty_alias (Mta_absent, path);
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc;
      }
    in
    let aliasable = not (Env.is_functor_arg path env) in
    let md =
      if alias && aliasable then md
      else
        match (Env.find_module path env).md_type with
        | Mty_alias (_, p1) when not alias ->
          let p1 = Env.normalize_path (Some smod.pmod_loc) env p1 in
          let mty = Includemod.expand_module_alias env [] p1 in
          {
            md with
            mod_desc =
              Tmod_constraint
                (md, mty, Tmodtype_implicit, Tcoerce_alias (p1, Tcoerce_none));
            mod_type =
              (if sttn then Mtype.strengthen ~aliasable:true env mty p1 else mty);
          }
        | mty ->
          let mty =
            if sttn then Mtype.strengthen ~aliasable env mty path else mty
          in
          {md with mod_type = mty}
    in
    rm md
  | Pmod_structure sstr ->
    let str, sg, _finalenv =
      type_structure funct_body anchor env sstr smod.pmod_loc
    in
    let md =
      rm
        {
          mod_desc = Tmod_structure str;
          mod_type = Mty_signature sg;
          mod_env = env;
          mod_attributes = smod.pmod_attributes;
          mod_loc = smod.pmod_loc;
        }
    in
    let sg' = simplify_signature sg in
    if List.length sg' = List.length sg then md
    else
      wrap_constraint
        (Env.implicit_coercion env)
        md (Mty_signature sg') Tmodtype_implicit
  | Pmod_functor (name, smty, sbody) ->
    let mty = may_map (transl_modtype env) smty in
    let ty_arg = may_map (fun m -> m.mty_type) mty in
    let (id, newenv), funct_body =
      match ty_arg with
      | None -> ((Ident.create "*", env), false)
      | Some mty -> (Env.enter_module ~arg:true name.txt mty env, true)
    in
    Ctype.init_def (Ident.current_time ());
    (* PR#6981 *)
    let body = type_module sttn funct_body None newenv sbody in
    rm
      {
        mod_desc = Tmod_functor (id, name, mty, body);
        mod_type = Mty_functor (id, ty_arg, body.mod_type);
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc;
      }
  | Pmod_apply (sfunct, sarg) -> (
    let arg = type_module true funct_body None env sarg in
    let path = path_of_module arg in
    let funct = type_module (sttn && path <> None) funct_body None env sfunct in
    match Env.scrape_alias env funct.mod_type with
    | Mty_functor (param, mty_param, mty_res) as mty_functor ->
      let generative, mty_param =
        (mty_param = None, Btype.default_mty mty_param)
      in
      if generative then (
        if sarg.pmod_desc <> Pmod_structure [] then
          raise (Error (sfunct.pmod_loc, env, Apply_generative));
        if funct_body && Mtype.contains_type env funct.mod_type then
          raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body)));
      let coercion =
        try Includemod.modtypes ~loc:sarg.pmod_loc env arg.mod_type mty_param
        with Includemod.Error msg ->
          raise (Error (sarg.pmod_loc, env, Not_included msg))
      in
      let mty_appl =
        match path with
        | Some path ->
          Subst.modtype (Subst.add_module param path Subst.identity) mty_res
        | None -> (
          if generative then mty_res
          else
            try
              Mtype.nondep_supertype
                (Env.add_module ~arg:true param arg.mod_type env)
                param mty_res
            with Not_found ->
              raise
                (Error
                   (smod.pmod_loc, env, Cannot_eliminate_dependency mty_functor))
          )
      in
      rm
        {
          mod_desc = Tmod_apply (funct, arg, coercion);
          mod_type = mty_appl;
          mod_env = env;
          mod_attributes = smod.pmod_attributes;
          mod_loc = smod.pmod_loc;
        }
    | Mty_alias (_, path) ->
      raise (Error (sfunct.pmod_loc, env, Cannot_scrape_alias path))
    | _ -> raise (Error (sfunct.pmod_loc, env, Cannot_apply funct.mod_type)))
  | Pmod_constraint (sarg, smty) ->
    let arg = type_module ~alias true funct_body anchor env sarg in
    let mty = transl_modtype env smty in
    rm
      {
        (wrap_constraint env arg mty.mty_type (Tmodtype_explicit mty)) with
        mod_loc = smod.pmod_loc;
        mod_attributes = smod.pmod_attributes;
      }
  | Pmod_unpack sexp ->
    let exp = Typecore.type_exp env sexp in
    let mty =
      match Ctype.expand_head env exp.exp_type with
      | {desc = Tpackage (p, nl, tl)} ->
        if List.exists (fun t -> Ctype.free_variables t <> []) tl then
          raise
            (Error (smod.pmod_loc, env, Incomplete_packed_module exp.exp_type));
        modtype_of_package env smod.pmod_loc p nl tl
      | {desc = Tvar _} ->
        raise
          (Typecore.Error (smod.pmod_loc, env, Typecore.Cannot_infer_signature))
      | _ ->
        raise (Error (smod.pmod_loc, env, Not_a_packed_module exp.exp_type))
    in
    if funct_body && Mtype.contains_type env mty then
      raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body));
    rm
      {
        mod_desc = Tmod_unpack (exp, mty);
        mod_type = mty;
        mod_env = env;
        mod_attributes = smod.pmod_attributes;
        mod_loc = smod.pmod_loc;
      }
  | Pmod_extension ext ->
    raise (Error_forward (Builtin_attributes.error_of_extension ext))

and type_structure ?(toplevel = false) funct_body anchor env sstr scope =
  let names = new_names () in

  let type_str_item env srem {pstr_loc = loc; pstr_desc = desc} =
    match desc with
    | Pstr_eval (sexpr, attrs) ->
      let expr =
        Builtin_attributes.warning_scope attrs (fun () ->
            Typecore.type_expression env sexpr)
      in
      (Tstr_eval (expr, attrs), [], env)
    | Pstr_value (rec_flag, sdefs) ->
      let scope =
        match rec_flag with
        | Recursive ->
          Some
            (Annot.Idef {scope with Location.loc_start = loc.Location.loc_start})
        | Nonrecursive ->
          let start =
            match srem with
            | [] -> loc.Location.loc_end
            | {pstr_loc = loc2} :: _ -> loc2.Location.loc_start
          in
          Some (Annot.Idef {scope with Location.loc_start = start})
      in
      let defs, newenv = Typecore.type_binding env rec_flag sdefs scope in
      let () =
        if rec_flag = Recursive then Rec_check.check_recursive_bindings defs
      in
      (* Note: Env.find_value does not trigger the value_used event. Values
         will be marked as being used during the signature inclusion test. *)
      ( Tstr_value (rec_flag, defs),
        List.map
          (fun id -> Sig_value (id, Env.find_value (Pident id) newenv))
          (let_bound_idents defs),
        newenv )
    | Pstr_primitive sdesc ->
      let desc, newenv = Typedecl.transl_value_decl env loc sdesc in
      (Tstr_primitive desc, [Sig_value (desc.val_id, desc.val_val)], newenv)
    | Pstr_type (rec_flag, sdecls) ->
      List.iter (fun decl -> check_name check_type names decl.ptype_name) sdecls;
      let decls, newenv = Typedecl.transl_type_decl env rec_flag sdecls in
      ( Tstr_type (rec_flag, decls),
        map_rec_type_with_row_types ~rec_flag
          (fun rs info -> Sig_type (info.typ_id, info.typ_type, rs))
          decls [],
        enrich_type_decls anchor decls env newenv )
    | Pstr_typext styext ->
      List.iter
        (fun pext -> check_name check_typext names pext.pext_name)
        styext.ptyext_constructors;
      let tyext, newenv = Typedecl.transl_type_extension true env loc styext in
      ( Tstr_typext tyext,
        map_ext
          (fun es ext -> Sig_typext (ext.ext_id, ext.ext_type, es))
          tyext.tyext_constructors [],
        newenv )
    | Pstr_exception sext ->
      check_name check_typext names sext.pext_name;
      let ext, newenv = Typedecl.transl_exception env sext in
      ( Tstr_exception ext,
        [Sig_typext (ext.ext_id, ext.ext_type, Text_exception)],
        newenv )
    | Pstr_module
        {pmb_name = name; pmb_expr = smodl; pmb_attributes = attrs; pmb_loc} ->
      check_name check_module names name;
      let id = Ident.create name.txt in
      (* create early for PR#6752 *)
      let modl =
        Builtin_attributes.warning_scope attrs (fun () ->
            type_module ~alias:true true funct_body
              (anchor_submodule name.txt anchor)
              env smodl)
      in
      let md =
        {
          md_type = enrich_module_type anchor name.txt modl.mod_type env;
          md_attributes = attrs;
          md_loc = pmb_loc;
        }
      in
      (*prerr_endline (Ident.unique_toplevel_name id);*)
      Mtype.lower_nongen (Ident.binding_time id - 1) md.md_type;
      let newenv = Env.enter_module_declaration id md env in
      ( Tstr_module
          {
            mb_id = id;
            mb_name = name;
            mb_expr = modl;
            mb_attributes = attrs;
            mb_loc = pmb_loc;
          },
        [
          Sig_module
            ( id,
              {md_type = modl.mod_type; md_attributes = attrs; md_loc = pmb_loc},
              Trec_not );
        ],
        newenv )
    | Pstr_recmodule sbind ->
      let sbind =
        List.map
          (function
            | {
                pmb_name = name;
                pmb_expr = {pmod_desc = Pmod_constraint (expr, typ)};
                pmb_attributes = attrs;
                pmb_loc = loc;
              } ->
              (name, typ, expr, attrs, loc)
            | mb ->
              raise
                (Error
                   ( mb.pmb_expr.pmod_loc,
                     env,
                     Recursive_module_require_explicit_type )))
          sbind
      in
      List.iter
        (fun (name, _, _, _, _) -> check_name check_module names name)
        sbind;
      let decls, newenv =
        transl_recmodule_modtypes env
          (List.map
             (fun (name, smty, _smodl, attrs, loc) ->
               {
                 pmd_name = name;
                 pmd_type = smty;
                 pmd_attributes = attrs;
                 pmd_loc = loc;
               })
             sbind)
      in
      let bindings1 =
        List.map2
          (fun {md_id = id; md_type = mty} (name, _, smodl, attrs, loc) ->
            let modl =
              Builtin_attributes.warning_scope attrs (fun () ->
                  type_module true funct_body (anchor_recmodule id) newenv smodl)
            in
            let mty' =
              enrich_module_type anchor (Ident.name id) modl.mod_type newenv
            in
            (id, name, mty, modl, mty', attrs, loc))
          decls sbind
      in
      let newenv =
        (* allow aliasing recursive modules from outside *)
        List.fold_left
          (fun env md ->
            let mdecl =
              {
                md_type = md.md_type.mty_type;
                md_attributes = md.md_attributes;
                md_loc = md.md_loc;
              }
            in
            Env.add_module_declaration ~check:true md.md_id mdecl env)
          env decls
      in
      let bindings2 = check_recmodule_inclusion newenv bindings1 in
      ( Tstr_recmodule bindings2,
        map_rec
          (fun rs mb ->
            Sig_module
              ( mb.mb_id,
                {
                  md_type = mb.mb_expr.mod_type;
                  md_attributes = mb.mb_attributes;
                  md_loc = mb.mb_loc;
                },
                rs ))
          bindings2 [],
        newenv )
    | Pstr_modtype pmtd ->
      (* check that it is non-abstract *)
      let newenv, mtd, sg = transl_modtype_decl names env pmtd in
      (Tstr_modtype mtd, [sg], newenv)
    | Pstr_open sod ->
      let _path, newenv, od = type_open ~toplevel env sod in
      (Tstr_open od, [], newenv)
    | Pstr_include sincl ->
      let smodl = sincl.pincl_mod in
      let modl =
        Builtin_attributes.warning_scope sincl.pincl_attributes (fun () ->
            type_module true funct_body None env smodl)
      in
      (* Rename all identifiers bound by this signature to avoid clashes *)
      let sg =
        Subst.signature Subst.identity
          (extract_sig_open env smodl.pmod_loc modl.mod_type)
      in
      List.iter (check_sig_item names loc) sg;
      let new_env = Env.add_signature sg env in
      let incl =
        {
          incl_mod = modl;
          incl_type = sg;
          incl_attributes = sincl.pincl_attributes;
          incl_loc = sincl.pincl_loc;
        }
      in
      (Tstr_include incl, sg, new_env)
    | Pstr_extension (ext, _attrs) ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))
    | Pstr_attribute x ->
      Builtin_attributes.warning_attribute x;
      (Tstr_attribute x, [], env)
  in
  let rec type_struct env sstr =
    Ctype.init_def (Ident.current_time ());
    match sstr with
    | [] -> ([], [], env)
    | pstr :: srem ->
      let previous_saved_types = Cmt_format.get_saved_types () in
      let desc, sg, new_env = type_str_item env srem pstr in
      let str = {str_desc = desc; str_loc = pstr.pstr_loc; str_env = env} in
      Cmt_format.set_saved_types
        (Cmt_format.Partial_structure_item str :: previous_saved_types);
      let str_rem, sig_rem, final_env = type_struct new_env srem in
      let new_sg = if rescript_hide desc then sig_rem else sg @ sig_rem in
      (str :: str_rem, new_sg, final_env)
  in
  if !Clflags.annotations then
    (* moved to genannot *)
    List.iter
      (function
        | {pstr_loc = l} -> Stypes.record_phrase l)
      sstr;
  let previous_saved_types = Cmt_format.get_saved_types () in
  let run () =
    let items, sg, final_env = type_struct env sstr in
    let str = {str_items = items; str_type = sg; str_final_env = final_env} in
    Cmt_format.set_saved_types
      (Cmt_format.Partial_structure str :: previous_saved_types);
    (str, sg, final_env)
  in
  if toplevel then run () else Builtin_attributes.warning_scope [] run

let type_toplevel_phrase env s =
  type_structure ~toplevel:true false None env s Location.none

let type_module_alias = type_module ~alias:true true false None
let type_module = type_module true false None
let type_structure = type_structure false None

(* Normalize types in a signature *)

let rec normalize_modtype env = function
  | Mty_ident _ | Mty_alias _ -> ()
  | Mty_signature sg -> normalize_signature env sg
  | Mty_functor (_id, _param, body) -> normalize_modtype env body

and normalize_signature env = List.iter (normalize_signature_item env)

and normalize_signature_item env = function
  | Sig_value (_id, desc) -> Ctype.normalize_type env desc.val_type
  | Sig_module (_id, md, _) -> normalize_modtype env md.md_type
  | _ -> ()

(* Extract the module type of a module expression *)

let type_module_type_of env smod =
  let tmty =
    match smod.pmod_desc with
    | Pmod_ident lid ->
      (* turn off strengthening in this case *)
      let path, md = Typetexp.find_module env smod.pmod_loc lid.txt in
      rm
        {
          mod_desc = Tmod_ident (path, lid);
          mod_type = md.md_type;
          mod_env = env;
          mod_attributes = smod.pmod_attributes;
          mod_loc = smod.pmod_loc;
        }
    | _ -> type_module env smod
  in
  let mty = tmty.mod_type in
  (* PR#6307: expand aliases at root and submodules *)
  let mty = Mtype.remove_aliases env mty in
  (* PR#5036: must not contain non-generalized type variables *)
  if not (closed_modtype env mty) then
    raise (Error (smod.pmod_loc, env, Non_generalizable_module mty));
  (tmty, mty)

(* For Typecore *)

let type_package env m p nl =
  (* Same as Pexp_letmodule *)
  (* remember original level *)
  let lv = Ctype.get_current_level () in
  Ctype.begin_def ();
  Ident.set_current_time lv;
  let context = Typetexp.narrow () in
  let modl = type_module env m in
  Ctype.init_def (Ident.current_time ());
  Typetexp.widen context;
  let mp, env =
    match modl.mod_desc with
    | Tmod_ident (mp, _) -> (mp, env)
    | Tmod_constraint ({mod_desc = Tmod_ident (mp, _)}, _, Tmodtype_implicit, _)
      ->
      (mp, env) (* PR#6982 *)
    | _ ->
      let id, new_env = Env.enter_module ~arg:true "%M" modl.mod_type env in
      (Pident id, new_env)
  in
  let rec mkpath mp = function
    | Lident name -> Pdot (mp, name, nopos)
    | Ldot (m, name) -> Pdot (mkpath mp m, name, nopos)
    | _ -> assert false
  in
  let tl' =
    List.map
      (fun name -> Btype.newgenty (Tconstr (mkpath mp name, [], ref Mnil)))
      (* beware of interactions with Printtyp and short-path:
         mp.name may have an arity > 0, cf. PR#7534 *)
      nl
  in
  (* go back to original level *)
  Ctype.end_def ();
  if nl = [] then (wrap_constraint env modl (Mty_ident p) Tmodtype_implicit, [])
  else
    let mty = modtype_of_package env modl.mod_loc p nl tl' in
    List.iter2
      (fun n ty ->
        try Ctype.unify env ty (Ctype.newvar ())
        with Ctype.Unify _ ->
          raise (Error (m.pmod_loc, env, Scoping_pack (n, ty))))
      nl tl';
    (wrap_constraint env modl mty Tmodtype_implicit, tl')

(* Fill in the forward declarations *)
let () =
  Typecore.type_module := type_module_alias;
  Typetexp.transl_modtype_longident := transl_modtype_longident;
  Typetexp.transl_modtype := transl_modtype;
  Typecore.type_open := type_open_ ?toplevel:None;
  Typecore.type_package := type_package;
  type_module_type_of_fwd := type_module_type_of

(* Typecheck an implementation file *)

let type_implementation_more ?check_exists sourcefile outputprefix modulename
    initial_env ast =
  Cmt_format.clear ();
  try
    Delayed_checks.reset_delayed_checks ();
    let str, sg, finalenv =
      type_structure initial_env ast (Location.in_file sourcefile)
    in
    let simple_sg = simplify_signature sg in
    let mli_status = !Clflags.assume_no_mli in
    if mli_status = Clflags.Mli_exists then (
      let intf_file =
        try find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
        with Not_found ->
          let sourceintf =
            Filename.remove_extension sourcefile ^ Literals.suffix_resi
          in
          raise
            (Error
               ( Location.in_file sourcefile,
                 Env.empty,
                 Interface_not_compiled sourceintf ))
      in
      let dclsig = Env.read_signature modulename intf_file in
      let coercion =
        Includemod.compunit initial_env sourcefile sg intf_file dclsig
      in
      Delayed_checks.force_delayed_checks ();
      (* It is important to run these checks after the inclusion test above,
         so that value declarations which are not used internally but exported
         are not reported as being unused. *)
      Cmt_format.save_cmt (outputprefix ^ ".cmt") modulename
        (Cmt_format.Implementation str) (Some sourcefile) initial_env None;
      (str, coercion, finalenv, dclsig)
      (* identifier is useless might read from serialized cmi files*))
    else
      let coercion =
        Includemod.compunit initial_env sourcefile sg "(inferred signature)"
          simple_sg
      in
      check_nongen_schemes finalenv simple_sg;
      normalize_signature finalenv simple_sg;
      Delayed_checks.force_delayed_checks ();
      (* See comment above. Here the target signature contains all
         the value being exported. We can still capture unused
         declarations like "let x = true;; let x = 1;;", because in this
         case, the inferred signature contains only the last declaration. *)
      (if not !Clflags.dont_write_files then
         let deprecated = Builtin_attributes.deprecated_of_str ast in
         let cmi =
           Env.save_signature ?check_exists ~deprecated simple_sg modulename
             (outputprefix ^ ".cmi")
         in
         Cmt_format.save_cmt (outputprefix ^ ".cmt") modulename
           (Cmt_format.Implementation str) (Some sourcefile) initial_env
           (Some cmi));
      (str, coercion, finalenv, simple_sg)
  with e ->
    Cmt_format.save_cmt (outputprefix ^ ".cmt") modulename
      (Cmt_format.Partial_implementation
         (Array.of_list (Cmt_format.get_saved_types ())))
      (Some sourcefile) initial_env None;
    raise e

let save_signature modname tsg outputprefix source_file initial_env cmi =
  Cmt_format.save_cmt (outputprefix ^ ".cmti") modname
    (Cmt_format.Interface tsg) (Some source_file) initial_env (Some cmi)

(* "Packaging" of several compilation units into one unit
   having them as sub-modules. *)

(* Error report *)

open Printtyp

let non_generalizable_msg ppf print_fallback_msg =
  fprintf ppf
    "%a@,\
     @,\
     @[This happens when the type system senses there's a \
     mutation/side-effect,@ in combination with a polymorphic value.@,\
     @{<info>Using or annotating that value usually solves it.@}@]"
    print_fallback_msg ()

let report_error ppf = function
  | Cannot_apply mty ->
    fprintf ppf "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
    fprintf ppf "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
    fprintf ppf
      "@[This functor has type@ %a@ The parameter cannot be eliminated in the \
       result type.@  Please bind the argument to a module identifier.@]"
      modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
    fprintf ppf "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
    fprintf ppf
      "@[The signature constrained by `with' has no component named %a@]"
      longident lid
  | With_mismatch (lid, explanation) ->
    fprintf ppf
      "@[<v>@[In this `with' constraint, the new definition of %a@ does not \
       match its original definition@ in the constrained signature:@]@ %a@]"
      longident lid Includemod.report_error explanation
  | With_makes_applicative_functor_ill_typed (lid, path, explanation) ->
    fprintf ppf
      "@[<v>@[This `with' constraint on %a makes the applicative functor @ \
       type %s ill-typed in the constrained signature:@]@ %a@]"
      longident lid (Path.name path) Includemod.report_error explanation
  | With_changes_module_alias (lid, id, path) ->
    fprintf ppf
      "@[<v>@[This `with' constraint on %a changes %s, which is aliased @ in \
       the constrained signature (as %s)@].@]"
      longident lid (Path.name path) (Ident.name id)
  | With_cannot_remove_constrained_type ->
    fprintf ppf
      "@[<v>Destructive substitutions are not supported for constrained @ \
       types (other than when replacing a type constructor with @ a type \
       constructor with the same arguments).@]"
  | Repeated_name (kind, name, repeated_loc) ->
    fprintf ppf
      "@[Multiple definition of the %s name %s @ at @{<loc>%a@}@ @ Names must \
       be unique in a given structure or signature.@]"
      kind name Location.print_loc repeated_loc
  | Non_generalizable typ ->
    (* modified *)
    fprintf ppf "@[<v>";
    non_generalizable_msg ppf (fun ppf () ->
        fprintf ppf
          "@[This expression's type contains type variables that cannot be \
           generalized:@,\
           @{<error>%a@}@]"
          type_scheme typ);
    fprintf ppf "@]"
  | Non_generalizable_module mty ->
    (* modified *)
    fprintf ppf "@[<v>";
    non_generalizable_msg ppf (fun ppf () ->
        fprintf ppf
          "@[The type of this module contains type variables that cannot be \
           generalized:@,\
           @{<error>%a@}@]"
          modtype mty);
    fprintf ppf "@]"
  | Interface_not_compiled intf_name ->
    fprintf ppf "@[Could not find the .cmi file for interface@ %a.@]"
      Location.print_filename intf_name
  | Not_allowed_in_functor_body ->
    fprintf ppf "@[This expression creates fresh types.@ %s@]"
      "It is not allowed inside applicative functors."
  | Not_a_packed_module ty ->
    fprintf ppf "This expression is not a packed module. It has type@ %a"
      type_expr ty
  | Incomplete_packed_module ty ->
    fprintf ppf "The type of this packed module contains variables:@ %a"
      type_expr ty
  | Scoping_pack (lid, ty) ->
    fprintf ppf "The type %a in this module cannot be exported.@ " longident lid;
    fprintf ppf "Its type contains local dependencies:@ %a" type_expr ty
  | Recursive_module_require_explicit_type ->
    fprintf ppf "Recursive modules require an explicit module type."
  | Apply_generative ->
    fprintf ppf "This is a generative functor. It can only be applied to ()"
  | Cannot_scrape_alias p ->
    fprintf ppf "This is an alias for module %a, which is missing" path p

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)

let () =
  Location.register_error_of_exn (function
    | Error (loc, env, err) ->
      Some (Location.error_of_printer loc (report_error env) err)
    | Error_forward err -> Some err
    | _ -> None)
