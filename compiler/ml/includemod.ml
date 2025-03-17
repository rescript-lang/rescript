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

(* Inclusion checks for the module language *)

open Misc
open Path
open Typedtree
open Types

type symptom =
  | Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of
      Ident.t
      * type_declaration
      * type_declaration
      * Includecore.type_mismatch list
  | Extension_constructors of
      Ident.t * extension_constructor * extension_constructor
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of Ident.t
  | Body of Ident.t
type error = pos list * Env.t * symptom

exception Error of error list

(* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions ~loc env cxt subst id vd1 vd2 =
  Cmt_format.record_value_dependency vd1 vd2;
  Env.mark_value_used env (Ident.name id) vd1;
  let vd2 = Subst.value_description subst vd2 in
  try Includecore.value_descriptions ~loc env id vd1 vd2
  with Includecore.Dont_match ->
    raise (Error [(cxt, env, Value_descriptions (id, vd1, vd2))])

(* Inclusion between type declarations *)

let type_declarations ~loc env ?(old_env = env) cxt subst id decl1 decl2 =
  Env.mark_type_used env (Ident.name id) decl1;
  let decl2 = Subst.type_declaration subst decl2 in
  let err =
    Includecore.type_declarations ~loc env (Ident.name id) decl1 id decl2
  in
  if err <> [] then
    raise (Error [(cxt, old_env, Type_declarations (id, decl1, decl2, err))])

(* Inclusion between extension constructors *)

let extension_constructors ~loc env cxt subst id ext1 ext2 =
  let ext2 = Subst.extension_constructor subst ext2 in
  if Includecore.extension_constructors ~loc env id ext1 ext2 then ()
  else raise (Error [(cxt, env, Extension_constructors (id, ext1, ext2))])

(* Expand a module type identifier when possible *)

exception Dont_match

let may_expand_module_path env path =
  try
    ignore (Env.find_modtype_expansion path env);
    true
  with Not_found -> false

let expand_module_path env cxt path =
  try Env.find_modtype_expansion path env
  with Not_found -> raise (Error [(cxt, env, Unbound_modtype_path path)])

let expand_module_alias env cxt path =
  try (Env.find_module path env).md_type
  with Not_found -> raise (Error [(cxt, env, Unbound_module_path path)])

(*
let rec normalize_module_path env cxt path =
  match expand_module_alias env cxt path with
    Mty_alias path' -> normalize_module_path env cxt path'
  | _ -> path
*)

(* Extract name, kind and ident from a signature item *)

type field_desc =
  | Field_value of string
  | Field_type of string
  | Field_typext of string
  | Field_module of string
  | Field_modtype of string

let kind_of_field_desc = function
  | Field_value _ -> "value"
  | Field_type _ -> "type"
  | Field_typext _ -> "extension constructor"
  | Field_module _ -> "module"
  | Field_modtype _ -> "module type"

let item_ident_name = function
  | Sig_value (id, d) -> (id, d.val_loc, Field_value (Ident.name id))
  | Sig_type (id, d, _) -> (id, d.type_loc, Field_type (Ident.name id))
  | Sig_typext (id, d, _) -> (id, d.ext_loc, Field_typext (Ident.name id))
  | Sig_module (id, d, _) -> (id, d.md_loc, Field_module (Ident.name id))
  | Sig_modtype (id, d) -> (id, d.mtd_loc, Field_modtype (Ident.name id))
  | Sig_class () -> assert false
  | Sig_class_type () -> assert false

let is_runtime_component = function
  | Sig_value (_, {val_kind = Val_prim _})
  | Sig_type (_, _, _)
  | Sig_modtype (_, _)
  | Sig_class_type () ->
    false
  | Sig_value (_, _)
  | Sig_typext (_, _, _)
  | Sig_module (_, _, _)
  | Sig_class () ->
    true

(* Print a coercion *)

let rec print_list pr ppf = function
  | [] -> ()
  | [a] -> pr ppf a
  | a :: l ->
    pr ppf a;
    Format.fprintf ppf ";@ ";
    print_list pr ppf l
let print_list pr ppf l = Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
  | Tcoerce_none -> pr "id"
  | Tcoerce_structure (fl, nl, _) ->
    pr "@[<2>struct@ %a@ %a@]"
      (print_list print_coercion2)
      fl
      (print_list print_coercion3)
      nl
  | Tcoerce_functor (inp, out) ->
    pr "@[<2>functor@ (%a)@ (%a)@]" print_coercion inp print_coercion out
  | Tcoerce_primitive {pc_desc; pc_env = _; pc_type} ->
    pr "prim %s@ (%a)" pc_desc.Primitive.prim_name Printtyp.raw_type_expr
      pc_type
  | Tcoerce_alias (p, c) ->
    pr "@[<2>alias %a@ (%a)@]" Printtyp.path p print_coercion c

and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c

and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]" (Ident.unique_name i) n print_coercion c

(* Simplify a structure coercion *)

let simplify_structure_coercion cc id_pos_list runtime_fields =
  let rec is_identity_coercion pos = function
    | [] -> true
    | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem
  in
  if is_identity_coercion 0 cc then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list, runtime_fields)

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let rec modtypes ~loc env cxt subst mty1 mty2 =
  try try_modtypes ~loc env cxt subst mty1 mty2 with
  | Dont_match ->
    raise (Error [(cxt, env, Module_types (mty1, Subst.modtype subst mty2))])
  | Error reasons as err -> (
    match (mty1, mty2) with
    | Mty_alias _, _ | _, Mty_alias _ -> raise err
    | _ ->
      raise
        (Error
           ((cxt, env, Module_types (mty1, Subst.modtype subst mty2)) :: reasons))
    )

and try_modtypes ~loc env cxt subst mty1 mty2 =
  match (mty1, mty2) with
  | Mty_alias (pres1, p1), Mty_alias (pres2, p2) -> (
    if Env.is_functor_arg p2 env then
      raise (Error [(cxt, env, Invalid_module_alias p2)]);
    (if not (Path.same p1 p2) then
       let p1 = Env.normalize_path None env p1
       and p2 = Env.normalize_path None env (Subst.module_path subst p2) in
       if not (Path.same p1 p2) then raise Dont_match);
    match (pres1, pres2) with
    | Mta_present, Mta_present ->
      Tcoerce_none (* Should really be Tcoerce_ignore if it existed *)
    | Mta_absent, Mta_absent ->
      Tcoerce_none (* Should really be Tcoerce_empty if it existed *)
    | Mta_present, Mta_absent -> Tcoerce_none
    | Mta_absent, Mta_present ->
      let p1 =
        try Env.normalize_path (Some Location.none) env p1
        with Env.Error (Env.Missing_module (_, _, path)) ->
          raise (Error [(cxt, env, Unbound_module_path path)])
      in
      Tcoerce_alias (p1, Tcoerce_none))
  | Mty_alias (pres1, p1), _ -> (
    let p1 =
      try Env.normalize_path (Some Location.none) env p1
      with Env.Error (Env.Missing_module (_, _, path)) ->
        raise (Error [(cxt, env, Unbound_module_path path)])
    in
    let mty1 =
      Mtype.strengthen ~aliasable:true env (expand_module_alias env cxt p1) p1
    in
    let cc = modtypes ~loc env cxt subst mty1 mty2 in
    match pres1 with
    | Mta_present -> cc
    | Mta_absent -> Tcoerce_alias (p1, cc))
  | Mty_ident p1, _ when may_expand_module_path env p1 ->
    try_modtypes ~loc env cxt subst (expand_module_path env cxt p1) mty2
  | _, Mty_ident _ -> try_modtypes2 ~loc env cxt mty1 (Subst.modtype subst mty2)
  | Mty_signature sig1, Mty_signature sig2 ->
    signatures ~loc env cxt subst sig1 sig2
  | Mty_functor (param1, None, res1), Mty_functor (_param2, None, res2) -> (
    match modtypes ~loc env (Body param1 :: cxt) subst res1 res2 with
    | Tcoerce_none -> Tcoerce_none
    | cc -> Tcoerce_functor (Tcoerce_none, cc))
  | Mty_functor (param1, Some arg1, res1), Mty_functor (param2, Some arg2, res2)
    -> (
    let arg2' = Subst.modtype subst arg2 in
    let cc_arg =
      modtypes ~loc env (Arg param1 :: cxt) Subst.identity arg2' arg1
    in
    let cc_res =
      modtypes ~loc
        (Env.add_module param1 arg2' env)
        (Body param1 :: cxt)
        (Subst.add_module param2 (Pident param1) subst)
        res1 res2
    in
    match (cc_arg, cc_res) with
    | Tcoerce_none, Tcoerce_none -> Tcoerce_none
    | _ -> Tcoerce_functor (cc_arg, cc_res))
  | _, _ -> raise Dont_match

and try_modtypes2 ~loc env cxt mty1 mty2 =
  (* mty2 is an identifier *)
  match (mty1, mty2) with
  | Mty_ident p1, Mty_ident p2
    when Path.same
           (Env.normalize_path_prefix None env p1)
           (Env.normalize_path_prefix None env p2) ->
    Tcoerce_none
  | _, Mty_ident p2 when may_expand_module_path env p2 ->
    try_modtypes ~loc env cxt Subst.identity mty1
      (expand_module_path env cxt p2)
  | _, _ -> raise Dont_match

(* Inclusion between signatures *)

and signatures ~loc env cxt subst sig1 sig2 =
  (* Environment used to check inclusion of components *)
  let new_env = Env.add_signature sig1 (Env.in_signature true env) in
  (* Keep ids for module aliases *)
  let id_pos_list, _ =
    List.fold_left
      (fun ((l, pos) as id_pos) -> function
        | Sig_module (id, _, _) -> ((id, pos, Tcoerce_none) :: l, pos + 1)
        | item -> if is_runtime_component item then (l, pos + 1) else id_pos)
      ([], 0) sig1
  in

  let runtime_fields =
    let get_id = function
      | Sig_value (i, _)
      | Sig_module (i, _, _)
      | Sig_typext (i, _, _)
      | Sig_modtype (i, _)
      | Sig_type (i, _, _) ->
        Ident.name i
      | Sig_class () | Sig_class_type () -> assert false
    in
    List.fold_right
      (fun item fields ->
        if is_runtime_component item then get_id item :: fields else fields)
      sig2 []
  in

  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
    | [] -> (pos, tbl)
    | item :: rem ->
      let id, _loc, name = item_ident_name item in
      let nextpos = if is_runtime_component item then pos + 1 else pos in
      build_component_table nextpos (Tbl.add name (id, item, pos) tbl) rem
  in
  let len1, comps1 = build_component_table 0 Tbl.empty sig1 in
  let len2 =
    List.fold_left
      (fun n i -> if is_runtime_component i then n + 1 else n)
      0 sig2
  in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
    | [] -> (
      match unpaired with
      | [] ->
        let cc =
          signature_components ~loc env new_env cxt subst (List.rev paired)
        in
        if len1 = len2 then
          (* see PR#5098 *)
          simplify_structure_coercion cc id_pos_list runtime_fields
        else Tcoerce_structure (cc, id_pos_list, runtime_fields)
      | _ -> raise (Error unpaired))
    | item2 :: rem -> (
      let id2, loc, name2 = item_ident_name item2 in
      let name2, report =
        match (item2, name2) with
        | Sig_type (_, {type_manifest = None}, _), Field_type s
          when Btype.is_row_name s ->
          (* Do not report in case of failure,
             as the main type will generate an error *)
          (Field_type (String.sub s 0 (String.length s - 4)), false)
        | _ -> (name2, true)
      in
      match Tbl.find name2 comps1 with
      | id1, item1, pos1 ->
        let new_subst =
          match item2 with
          | Sig_type _ -> Subst.add_type id2 (Pident id1) subst
          | Sig_module _ -> Subst.add_module id2 (Pident id1) subst
          | Sig_modtype _ ->
            Subst.add_modtype id2 (Mty_ident (Pident id1)) subst
          | Sig_value _ | Sig_typext _ | Sig_class _ | Sig_class_type () ->
            subst
        in
        pair_components new_subst ((item1, item2, pos1) :: paired) unpaired rem
      | exception Not_found ->
        let unpaired =
          if report then
            (cxt, env, Missing_field (id2, loc, kind_of_field_desc name2))
            :: unpaired
          else unpaired
        in
        pair_components subst paired unpaired rem)
  in
  (* Do the pairing and checking, and return the final coercion *)
  pair_components subst [] [] sig2

(* Inclusion between signature components *)

and signature_components ~loc old_env env cxt subst paired =
  let comps_rec rem = signature_components ~loc old_env env cxt subst rem in
  match paired with
  | [] -> []
  | (Sig_value (id1, valdecl1), Sig_value (_id2, valdecl2), pos) :: rem -> (
    let cc = value_descriptions ~loc env cxt subst id1 valdecl1 valdecl2 in
    match valdecl2.val_kind with
    | Val_prim _ -> comps_rec rem
    | _ -> (pos, cc) :: comps_rec rem)
  | (Sig_type (id1, tydecl1, _), Sig_type (_id2, tydecl2, _), _pos) :: rem ->
    type_declarations ~loc ~old_env env cxt subst id1 tydecl1 tydecl2;
    comps_rec rem
  | (Sig_typext (id1, ext1, _), Sig_typext (_id2, ext2, _), pos) :: rem ->
    extension_constructors ~loc env cxt subst id1 ext1 ext2;
    (pos, Tcoerce_none) :: comps_rec rem
  | (Sig_module (id1, mty1, _), Sig_module (_id2, mty2, _), pos) :: rem ->
    let cc = module_declarations ~loc env cxt subst id1 mty1 mty2 in
    (pos, cc) :: comps_rec rem
  | (Sig_modtype (id1, info1), Sig_modtype (_id2, info2), _pos) :: rem ->
    modtype_infos ~loc env cxt subst id1 info1 info2;
    comps_rec rem
  | (Sig_class (), Sig_class (), _) :: _ -> assert false
  | (Sig_class_type (), Sig_class_type (), _pos) :: _ -> assert false
  | _ -> assert false

and module_declarations ~loc env cxt subst id1 md1 md2 =
  Builtin_attributes.check_deprecated_inclusion ~def:md1.md_loc ~use:md2.md_loc
    loc md1.md_attributes md2.md_attributes (Ident.name id1);
  let p1 = Pident id1 in
  Env.mark_module_used env (Ident.name id1) md1.md_loc;
  modtypes ~loc env (Module id1 :: cxt) subst
    (Mtype.strengthen ~aliasable:true env md1.md_type p1)
    md2.md_type

(* Inclusion between module type specifications *)

and modtype_infos ~loc env cxt subst id info1 info2 =
  Builtin_attributes.check_deprecated_inclusion ~def:info1.mtd_loc
    ~use:info2.mtd_loc loc info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.modtype_declaration subst info2 in
  let cxt' = Modtype id :: cxt in
  try
    match (info1.mtd_type, info2.mtd_type) with
    | None, None -> ()
    | Some _, None -> ()
    | Some mty1, Some mty2 -> check_modtype_equiv ~loc env cxt' mty1 mty2
    | None, Some mty2 ->
      check_modtype_equiv ~loc env cxt' (Mty_ident (Pident id)) mty2
  with Error reasons ->
    raise (Error ((cxt, env, Modtype_infos (id, info1, info2)) :: reasons))

and check_modtype_equiv ~loc env cxt mty1 mty2 =
  match
    ( modtypes ~loc env cxt Subst.identity mty1 mty2,
      modtypes ~loc env cxt Subst.identity mty2 mty1 )
  with
  | Tcoerce_none, Tcoerce_none -> ()
  | _c1, _c2 ->
    (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
       print_coercion _c1 print_coercion _c2; *)
    raise (Error [(cxt, env, Modtype_permutation)])

(* Simplified inclusion check between module types (for Env) *)

let can_alias env path =
  let rec no_apply = function
    | Pident _ -> true
    | Pdot (p, _, _) -> no_apply p
    | Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  try
    let aliasable = can_alias env path1 in
    ignore
      (modtypes ~loc env [] Subst.identity
         (Mtype.strengthen ~aliasable env mty1 path1)
         mty2)
  with Error _ -> raise Not_found

let _ = Env.check_modtype_inclusion := check_modtype_inclusion

(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit env impl_name impl_sig intf_name intf_sig =
  try
    signatures
      ~loc:(Location.in_file impl_name)
      env [] Subst.identity impl_sig intf_sig
  with Error reasons ->
    raise
      (Error
         (([], Env.empty, Interface_mismatch (impl_name, intf_name)) :: reasons))

(* Hide the context and substitution parameters to the outside world *)

let modtypes ~loc env mty1 mty2 = modtypes ~loc env [] Subst.identity mty1 mty2
let signatures env sig1 sig2 =
  signatures ~loc:Location.none env [] Subst.identity sig1 sig2
let type_declarations ~loc env id decl1 decl2 =
  type_declarations ~loc env [] Subst.identity id decl1 decl2

(*
let modtypes env m1 m2 =
  let c = modtypes env m1 m2 in
  Format.eprintf "@[<2>modtypes@ %a@ %a =@ %a@]@."
    Printtyp.modtype m1 Printtyp.modtype m2
    print_coercion c;
  c
*)

(* Error report *)

open Format
open Printtyp

let show_loc msg ppf loc =
  fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1

let include_err ppf = function
  | Missing_field (id, loc, kind) ->
    fprintf ppf "The %s `%a' is required but not provided" kind ident id;
    show_loc "Expected declaration" ppf loc
  | Value_descriptions (id, d1, d2) ->
    let curry_kind_1, curry_kind_2 = ("", "") in
    fprintf ppf
      "@[<hv 2>Values do not match:@ %a%s@;<1 -2>is not included in@ %a%s@]"
      (value_description id) d1 curry_kind_1 (value_description id) d2
      curry_kind_2;
    show_locs ppf (d1.val_loc, d2.val_loc)
  | Type_declarations (id, d1, d2, errs) ->
    fprintf ppf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a@]"
      "Type declarations do not match" (type_declaration id) d1
      "is not included in" (type_declaration id) d2 show_locs
      (d1.type_loc, d2.type_loc)
      (Includecore.report_type_mismatch "the first" "the second" "declaration")
      errs
  | Extension_constructors (id, x1, x2) ->
    fprintf ppf
      "@[<hv 2>Extension declarations do not match:@ %a@;\
       <1 -2>is not included in@ %a@]"
      (extension_constructor id) x1 (extension_constructor id) x2;
    show_locs ppf (x1.ext_loc, x2.ext_loc)
  | Module_types (mty1, mty2) ->
    fprintf ppf
      "@[<hv 2>Modules do not match:@ %a@;<1 -2>is not included in@ %a@]"
      modtype mty1 modtype mty2
  | Modtype_infos (id, d1, d2) ->
    fprintf ppf
      "@[<hv 2>Module type declarations do not match:@ %a@;\
       <1 -2>does not match@ %a@]"
      (modtype_declaration id) d1 (modtype_declaration id) d2
  | Modtype_permutation -> fprintf ppf "Illegal permutation of structure fields"
  | Interface_mismatch (impl_name, intf_name) ->
    fprintf ppf "@[The implementation %s@ does not match the interface %s:"
      impl_name intf_name
  | Unbound_modtype_path path ->
    fprintf ppf "Unbound module type %a" Printtyp.path path
  | Unbound_module_path path ->
    fprintf ppf "Unbound module %a" Printtyp.path path
  | Invalid_module_alias path ->
    fprintf ppf "Module %a cannot be aliased" Printtyp.path path

let rec context ppf = function
  | Module id :: rem -> fprintf ppf "@[<2>module %a%a@]" ident id args rem
  | Modtype id :: rem ->
    fprintf ppf "@[<2>module type %a =@ %a@]" ident id context_mty rem
  | Body x :: rem ->
    fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
  | Arg x :: rem ->
    fprintf ppf "functor (%a : %a) -> ..." ident x context_mty rem
  | [] -> fprintf ppf "<here>"

and context_mty ppf = function
  | (Module _ | Modtype _) :: _ as rem ->
    fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
  | cxt -> context ppf cxt

and args ppf = function
  | Body x :: rem -> fprintf ppf "(%s)%a" (argname x) args rem
  | Arg x :: rem -> fprintf ppf "(%a :@ %a) : ..." ident x context_mty rem
  | cxt -> fprintf ppf " :@ %a" context_mty cxt

and argname x =
  let s = Ident.name x in
  if s = "*" then "" else s

let path_of_context = function
  | Module id :: rem ->
    let rec subm path = function
      | [] -> path
      | Module id :: rem -> subm (Pdot (path, Ident.name id, -1)) rem
      | _ -> assert false
    in
    subm (Pident id) rem
  | _ -> assert false

let context ppf cxt =
  if cxt = [] then ()
  else if
    List.for_all
      (function
        | Module _ -> true
        | _ -> false)
      cxt
  then fprintf ppf "In module %a:@ " path (path_of_context cxt)
  else fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt

let include_err ppf (cxt, env, err) =
  Printtyp.wrap_printing_env env (fun () ->
      fprintf ppf "@[<v>%a%a@]" context (List.rev cxt) include_err err)

let buffer = ref Bytes.empty
let is_big obj =
  let size = !Clflags.error_size in
  size > 0
  &&
  (if Bytes.length !buffer < size then buffer := Bytes.create size;
   try
     ignore (Marshal.to_buffer !buffer 0 size obj []);
     false
   with _ -> true)

let report_error ppf errs =
  if errs = [] then ()
  else
    let errs, err = split_last errs in
    let pe = ref true in
    let include_err' ppf ((_, _, obj) as err) =
      if not (is_big obj) then fprintf ppf "%a@ " include_err err
      else if !pe then (
        fprintf ppf "...@ ";
        pe := false)
    in
    let print_errs ppf = List.iter (include_err' ppf) in
    fprintf ppf "@[<v>%a%a@]" print_errs errs include_err err

let better_candidate_loc (x : error list) =
  match x with
  | [(_, _, Interface_mismatch _); (_, _, descr)] -> (
    match descr with
    | Value_descriptions (_, d1, _) -> Some d1.val_loc
    | Type_declarations (_, tdcl1, _, _) -> Some tdcl1.type_loc
    | Missing_field (_, loc, _) -> Some loc
    | _ -> None)
  | _ -> None

(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let () =
  Location.register_error_of_exn (function
    | Error err -> (
      match better_candidate_loc err with
      | None -> Some (Location.error_of_printer_file report_error err)
      | Some loc -> Some (Location.error_of_printer loc report_error err))
    | _ -> None)
