(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Location
open Longident
open Parsetree

let pp_deps = ref []

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)
module StringMap = Map.Make (String)

(* Module resolution map *)
(* Node (set of imports for this path, map for submodules) *)
type map_tree = Node of StringSet.t * bound_map
and bound_map = map_tree StringMap.t
let bound = Node (StringSet.empty, StringMap.empty)

(*let get_free (Node (s, _m)) = s*)
let get_map (Node (_s, m)) = m
let make_leaf s = Node (StringSet.singleton s, StringMap.empty)
let make_node m = Node (StringSet.empty, m)
let rec weaken_map s (Node (s0, m0)) =
  Node (StringSet.union s s0, StringMap.map (weaken_map s) m0)
let rec collect_free (Node (s, m)) =
  StringMap.fold (fun _ n -> StringSet.union (collect_free n)) m s

(* Returns the imports required to access the structure at path p *)
(* Only raises Not_found if the head of p is not in the toplevel map *)
let rec lookup_free p m =
  match p with
  | [] -> raise Not_found
  | s :: p -> (
    let (Node (f, m')) = StringMap.find s m in
    try lookup_free p m' with Not_found -> f)

(* Returns the node corresponding to the structure at path p *)
let rec lookup_map lid m =
  match lid with
  | Lident s -> StringMap.find s m
  | Ldot (l, s) -> StringMap.find s (get_map (lookup_map l m))
  | Lapply _ -> raise Not_found

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let add_names s =
  free_structure_names := StringSet.union s !free_structure_names

let rec add_path bv ?(p = []) = function
  | Lident s ->
    let free =
      try lookup_free (s :: p) bv with Not_found -> StringSet.singleton s
    in
    (*StringSet.iter (fun s -> Printf.eprintf "%s " s) free;
      prerr_endline "";*)
    add_names free
  | Ldot (l, s) -> add_path bv ~p:(s :: p) l
  | Lapply (l1, l2) ->
    add_path bv l1;
    add_path bv l2

let open_module bv lid =
  match lookup_map lid bv with
  | Node (s, m) ->
    add_names s;
    StringMap.fold StringMap.add m bv
  | exception Not_found ->
    add_path bv lid;
    bv

let add_parent bv lid =
  match lid.txt with
  | Ldot (l, _s) -> add_path bv l
  | _ -> ()

let add = add_parent

let addmodule bv lid = add_path bv lid.txt

let handle_extension ext =
  match (fst ext).txt with
  | "error" | "ocaml.error" ->
    raise (Location.Error (Builtin_attributes.error_of_extension ext))
  | _ -> ()

let rec add_type bv ty =
  match ty.ptyp_desc with
  | Ptyp_any -> ()
  | Ptyp_var _ -> ()
  | Ptyp_arrow (_, t1, t2, _) ->
    add_type bv t1;
    add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr (c, tl) ->
    add bv c;
    List.iter (add_type bv) tl
  | Ptyp_object (fl, _) ->
    List.iter
      (function
        | Otag (_, _, t) -> add_type bv t
        | Oinherit t -> add_type bv t)
      fl
  | Ptyp_alias (t, _) -> add_type bv t
  | Ptyp_variant (fl, _, _) ->
    List.iter
      (function
        | Rtag (_, _, _, stl) -> List.iter (add_type bv) stl
        | Rinherit sty -> add_type bv sty)
      fl
  | Ptyp_poly (_, t) -> add_type bv t
  | Ptyp_package pt -> add_package_type bv pt
  | Ptyp_extension e -> handle_extension e

and add_package_type bv (lid, l) =
  add bv lid;
  List.iter (add_type bv) (List.map (fun (_, e) -> e) l)

let add_opt add_fn bv = function
  | None -> ()
  | Some x -> add_fn bv x

let add_constructor_arguments bv = function
  | Pcstr_tuple l -> List.iter (add_type bv) l
  | Pcstr_record l -> List.iter (fun l -> add_type bv l.pld_type) l

let add_constructor_decl bv pcd =
  add_constructor_arguments bv pcd.pcd_args;
  Misc.may (add_type bv) pcd.pcd_res

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) ->
      add_type bv ty1;
      add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  let add_tkind = function
    | Ptype_abstract -> ()
    | Ptype_variant cstrs -> List.iter (add_constructor_decl bv) cstrs
    | Ptype_record lbls -> List.iter (fun pld -> add_type bv pld.pld_type) lbls
    | Ptype_open -> ()
  in
  add_tkind td.ptype_kind

let add_extension_constructor bv ext =
  match ext.pext_kind with
  | Pext_decl (args, rty) ->
    add_constructor_arguments bv args;
    Misc.may (add_type bv) rty
  | Pext_rebind lid -> add bv lid

let add_type_extension bv te =
  add bv te.ptyext_path;
  List.iter (add_extension_constructor bv) te.ptyext_constructors

let pattern_bv = ref StringMap.empty

let rec add_pattern bv pat =
  match pat.ppat_desc with
  | Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias (p, _) -> add_pattern bv p
  | Ppat_interval _ | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct (c, op) ->
    add bv c;
    add_opt add_pattern bv op
  | Ppat_record (pl, _) ->
    List.iter
      (fun (lbl, p, _) ->
        add bv lbl;
        add_pattern bv p)
      pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or (p1, p2) ->
    add_pattern bv p1;
    add_pattern bv p2
  | Ppat_constraint (p, ty) ->
    add_pattern bv p;
    add_type bv ty
  | Ppat_variant (_, op) -> add_opt add_pattern bv op
  | Ppat_type li -> add bv li
  | Ppat_lazy p -> add_pattern bv p
  | Ppat_unpack id -> pattern_bv := StringMap.add id.txt bound !pattern_bv
  | Ppat_open (m, p) ->
    let bv = open_module bv m.txt in
    add_pattern bv p
  | Ppat_exception p -> add_pattern bv p
  | Ppat_extension e -> handle_extension e

let add_pattern bv pat =
  pattern_bv := bv;
  add_pattern bv pat;
  !pattern_bv

let rec add_expr bv exp =
  match exp.pexp_desc with
  | Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let (rf, pel, e) ->
    let bv = add_bindings rf bv pel in
    add_expr bv e
  | Pexp_fun {default = opte; lhs = p; rhs = e} ->
    add_opt add_expr bv opte;
    add_expr (add_pattern bv p) e
  | Pexp_apply (e, el) ->
    add_expr bv e;
    List.iter (fun (_, e) -> add_expr bv e) el
  | Pexp_match (e, pel) ->
    add_expr bv e;
    add_cases bv pel
  | Pexp_try (e, pel) ->
    add_expr bv e;
    add_cases bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct (c, opte) ->
    add bv c;
    add_opt add_expr bv opte
  | Pexp_variant (_, opte) -> add_opt add_expr bv opte
  | Pexp_record (lblel, opte) ->
    List.iter
      (fun (lbl, e, _) ->
        add bv lbl;
        add_expr bv e)
      lblel;
    add_opt add_expr bv opte
  | Pexp_field (e, fld) ->
    add_expr bv e;
    add bv fld
  | Pexp_setfield (e1, fld, e2) ->
    add_expr bv e1;
    add bv fld;
    add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse (e1, e2, opte3) ->
    add_expr bv e1;
    add_expr bv e2;
    add_opt add_expr bv opte3
  | Pexp_sequence (e1, e2) ->
    add_expr bv e1;
    add_expr bv e2
  | Pexp_while (e1, e2) ->
    add_expr bv e1;
    add_expr bv e2
  | Pexp_for (_, e1, e2, _, e3) ->
    add_expr bv e1;
    add_expr bv e2;
    add_expr bv e3
  | Pexp_coerce (e1, (), ty3) ->
    add_expr bv e1;
    add_type bv ty3
  | Pexp_constraint (e1, ty2) ->
    add_expr bv e1;
    add_type bv ty2
  | Pexp_send (e, _m) -> add_expr bv e
  | Pexp_new li -> add bv li
  | Pexp_setinstvar (_v, e) -> add_expr bv e
  | Pexp_override sel -> List.iter (fun (_s, e) -> add_expr bv e) sel
  | Pexp_letmodule (id, m, e) ->
    let b = add_module_binding bv m in
    add_expr (StringMap.add id.txt b bv) e
  | Pexp_letexception (_, e) -> add_expr bv e
  | Pexp_assert e -> add_expr bv e
  | Pexp_lazy e -> add_expr bv e
  | Pexp_poly (e, t) ->
    add_expr bv e;
    add_opt add_type bv t
  | Pexp_newtype (_, e) -> add_expr bv e
  | Pexp_pack m -> add_module bv m
  | Pexp_open (_ovf, m, e) ->
    let bv = open_module bv m.txt in
    add_expr bv e
  | Pexp_extension
      (( {txt = "ocaml.extension_constructor" | "extension_constructor"; _},
         PStr [item] ) as e) -> (
    match item.pstr_desc with
    | Pstr_eval ({pexp_desc = Pexp_construct (c, None)}, _) -> add bv c
    | _ -> handle_extension e)
  | Pexp_extension e -> handle_extension e

and add_cases bv cases = List.iter (add_case bv) cases

and add_case bv {pc_lhs; pc_guard; pc_rhs} =
  let bv = add_pattern bv pc_lhs in
  add_opt add_expr bv pc_guard;
  add_expr bv pc_rhs

and add_bindings recf bv pel =
  let bv' = List.fold_left (fun bv x -> add_pattern bv x.pvb_pat) bv pel in
  let bv = if recf = Recursive then bv' else bv in
  List.iter (fun x -> add_expr bv x.pvb_expr) pel;
  bv'

and add_modtype bv mty =
  match mty.pmty_desc with
  | Pmty_ident l -> add bv l
  | Pmty_alias l -> addmodule bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor (id, mty1, mty2) ->
    Misc.may (add_modtype bv) mty1;
    add_modtype (StringMap.add id.txt bound bv) mty2
  | Pmty_with (mty, cstrl) ->
    add_modtype bv mty;
    List.iter
      (function
        | Pwith_type (_, td) -> add_type_declaration bv td
        | Pwith_module (_, lid) -> addmodule bv lid
        | Pwith_typesubst (_, td) -> add_type_declaration bv td
        | Pwith_modsubst (_, lid) -> addmodule bv lid)
      cstrl
  | Pmty_typeof m -> add_module bv m
  | Pmty_extension e -> handle_extension e

and add_module_alias bv l =
  try
    add_parent bv l;
    lookup_map l.txt bv
  with Not_found -> (
    match l.txt with
    | Lident s -> make_leaf s
    | _ ->
      addmodule bv l;
      bound (* cannot delay *))

and add_modtype_binding bv mty =
  if not !Clflags.transparent_modules then add_modtype bv mty;
  match mty.pmty_desc with
  | Pmty_alias l -> add_module_alias bv l
  | Pmty_signature s -> make_node (add_signature_binding bv s)
  | Pmty_typeof modl -> add_module_binding bv modl
  | _ ->
    if !Clflags.transparent_modules then add_modtype bv mty;
    bound

and add_signature bv sg = ignore (add_signature_binding bv sg)

and add_signature_binding bv sg =
  snd (List.fold_left add_sig_item (bv, StringMap.empty) sg)

and add_sig_item (bv, m) item =
  match item.psig_desc with
  | Psig_value vd ->
    add_type bv vd.pval_type;
    (bv, m)
  | Psig_type (_, dcls) ->
    List.iter (add_type_declaration bv) dcls;
    (bv, m)
  | Psig_typext te ->
    add_type_extension bv te;
    (bv, m)
  | Psig_exception pext ->
    add_extension_constructor bv pext;
    (bv, m)
  | Psig_module pmd ->
    let m' = add_modtype_binding bv pmd.pmd_type in
    let add = StringMap.add pmd.pmd_name.txt m' in
    (add bv, add m)
  | Psig_recmodule decls ->
    let add =
      List.fold_right (fun pmd -> StringMap.add pmd.pmd_name.txt bound) decls
    in
    let bv' = add bv and m' = add m in
    List.iter (fun pmd -> add_modtype bv' pmd.pmd_type) decls;
    (bv', m')
  | Psig_modtype x ->
    (match x.pmtd_type with
    | None -> ()
    | Some mty -> add_modtype bv mty);
    (bv, m)
  | Psig_open od -> (open_module bv od.popen_lid.txt, m)
  | Psig_include incl ->
    let (Node (s, m')) = add_modtype_binding bv incl.pincl_mod in
    add_names s;
    let add = StringMap.fold StringMap.add m' in
    (add bv, add m)
  | Psig_class () -> (bv, m)
  | Psig_class_type () -> (bv, m)
  | Psig_attribute _ -> (bv, m)
  | Psig_extension (e, _) ->
    handle_extension e;
    (bv, m)

and add_module_binding bv modl =
  if not !Clflags.transparent_modules then add_module bv modl;
  match modl.pmod_desc with
  | Pmod_ident l -> (
    try
      add_parent bv l;
      lookup_map l.txt bv
    with Not_found -> (
      match l.txt with
      | Lident s -> make_leaf s
      | _ ->
        addmodule bv l;
        bound))
  | Pmod_structure s -> make_node (snd (add_structure_binding bv s))
  | _ ->
    if !Clflags.transparent_modules then add_module bv modl;
    bound

and add_module bv modl =
  match modl.pmod_desc with
  | Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> ignore (add_structure bv s)
  | Pmod_functor (id, mty, modl) ->
    Misc.may (add_modtype bv) mty;
    add_module (StringMap.add id.txt bound bv) modl
  | Pmod_apply (mod1, mod2) ->
    add_module bv mod1;
    add_module bv mod2
  | Pmod_constraint (modl, mty) ->
    add_module bv modl;
    add_modtype bv mty
  | Pmod_unpack e -> add_expr bv e
  | Pmod_extension e -> handle_extension e

and add_structure bv item_list =
  let bv, m = add_structure_binding bv item_list in
  add_names (collect_free (make_node m));
  bv

and add_structure_binding bv item_list =
  List.fold_left add_struct_item (bv, StringMap.empty) item_list

and add_struct_item (bv, m) item : _ StringMap.t * _ StringMap.t =
  match item.pstr_desc with
  | Pstr_eval (e, _attrs) ->
    add_expr bv e;
    (bv, m)
  | Pstr_value (rf, pel) ->
    let bv = add_bindings rf bv pel in
    (bv, m)
  | Pstr_primitive vd ->
    add_type bv vd.pval_type;
    (bv, m)
  | Pstr_type (_, dcls) ->
    List.iter (add_type_declaration bv) dcls;
    (bv, m)
  | Pstr_typext te ->
    add_type_extension bv te;
    (bv, m)
  | Pstr_exception pext ->
    add_extension_constructor bv pext;
    (bv, m)
  | Pstr_module x ->
    let b = add_module_binding bv x.pmb_expr in
    let add = StringMap.add x.pmb_name.txt b in
    (add bv, add m)
  | Pstr_recmodule bindings ->
    let add =
      List.fold_right (fun x -> StringMap.add x.pmb_name.txt bound) bindings
    in
    let bv' = add bv and m = add m in
    List.iter (fun x -> add_module bv' x.pmb_expr) bindings;
    (bv', m)
  | Pstr_modtype x ->
    (match x.pmtd_type with
    | None -> ()
    | Some mty -> add_modtype bv mty);
    (bv, m)
  | Pstr_open od -> (open_module bv od.popen_lid.txt, m)
  | Pstr_class () -> (bv, m)
  | Pstr_class_type () -> (bv, m)
  | Pstr_include incl ->
    let (Node (s, m')) = add_module_binding bv incl.pincl_mod in
    add_names s;
    let add = StringMap.fold StringMap.add m' in
    (add bv, add m)
  | Pstr_attribute _ -> (bv, m)
  | Pstr_extension (e, _) ->
    handle_extension e;
    (bv, m)

and add_implementation bv l =
  if !Clflags.transparent_modules then ignore (add_structure_binding bv l)
  else ignore (add_structure bv l)

and add_implementation_binding bv l = snd (add_structure_binding bv l)
