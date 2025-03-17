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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree

type error = Fragile_pattern_in_toplevel

exception Error of Location.t * error

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming extensions. *)

let global_path glob : Path.t option = Some (Pident glob)

let is_top (rootpath : Path.t option) =
  match rootpath with
  | Some (Pident _) -> true
  | _ -> false

let functor_path path param : Path.t option =
  match path with
  | None -> None
  | Some p -> Some (Papply (p, Pident param))

let field_path path field : Path.t option =
  match path with
  | None -> None
  | Some p -> Some (Pdot (p, Ident.name field, Path.nopos))

(* Compile type extensions *)

let transl_type_extension env rootpath (tyext : Typedtree.type_extension) body :
    Lambda.lambda =
  List.fold_right
    (fun ext body ->
      let lam =
        Translcore.transl_extension_constructor env
          (field_path rootpath ext.ext_id)
          ext
      in
      Lambda.Llet (Strict, Pgenval, ext.ext_id, lam, body))
    tyext.tyext_constructors body

(* Compile a coercion *)

let rec apply_coercion loc strict (restr : Typedtree.module_coercion) arg =
  match restr with
  | Tcoerce_none -> arg
  | Tcoerce_structure (pos_cc_list, id_pos_list, runtime_fields) ->
    Lambda.name_lambda strict arg (fun id ->
        let get_field_name name pos =
          Lambda.Lprim (Pfield (pos, Fld_module {name}), [Lvar id], loc)
        in
        let lam =
          Lambda.Lprim
            ( Pmakeblock (Blk_module runtime_fields),
              Ext_list.map2 pos_cc_list runtime_fields (fun (pos, cc) name ->
                  apply_coercion loc Alias cc
                    (Lprim (Pfield (pos, Fld_module {name}), [Lvar id], loc))),
              loc )
        in
        wrap_id_pos_list loc id_pos_list get_field_name lam)
  | Tcoerce_functor (cc_arg, cc_res) ->
    let param = Ident.create "funarg" in
    let carg = apply_coercion loc Alias cc_arg (Lvar param) in
    apply_coercion_result loc strict arg param carg cc_res
  | Tcoerce_primitive {pc_loc; pc_desc; pc_env; pc_type} ->
    Translcore.transl_primitive pc_loc pc_desc pc_env pc_type
  | Tcoerce_alias (path, cc) ->
    Lambda.name_lambda strict arg (fun _ ->
        apply_coercion loc Alias cc (Lambda.transl_normal_path path))

and apply_coercion_result loc strict funct param arg cc_res =
  Lambda.name_lambda strict funct (fun id ->
      Lfunction
        {
          params = [param];
          attr = {Lambda.default_function_attribute with is_a_functor = true};
          loc;
          body =
            apply_coercion loc Strict cc_res
              (Lapply
                 {
                   ap_loc = loc;
                   ap_func = Lvar id;
                   ap_args = [arg];
                   ap_inlined = Default_inline;
                 });
        })

and wrap_id_pos_list loc id_pos_list get_field lam =
  let fv = Lambda.free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
    IdentSet.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
    Format.eprintf "@.";*)
  let lam, s =
    List.fold_left
      (fun (lam, s) (id', pos, c) ->
        if Lambda.IdentSet.mem id' fv then
          let id'' = Ident.create (Ident.name id') in
          ( Lambda.Llet
              ( Alias,
                Pgenval,
                id'',
                apply_coercion loc Alias c (get_field (Ident.name id') pos),
                lam ),
            Ident.add id' (Lambda.Lvar id'') s )
        else (lam, s))
      (lam, Ident.empty) id_pos_list
  in
  if s == Ident.empty then lam else Lambda.subst_lambda s lam

(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
  | Tcoerce_none, c2 -> c2
  | c1, Tcoerce_none -> c1
  | ( Tcoerce_structure (pc1, ids1, runtime_fields1),
      Tcoerce_structure (pc2, ids2, _runtime_fields2) ) ->
    let v2 = Array.of_list pc2 in
    let ids1 =
      List.map
        (fun (id, pos1, c1) ->
          let pos2, c2 = v2.(pos1) in
          (id, pos2, compose_coercions c1 c2))
        ids1
    in
    Tcoerce_structure
      ( List.map
          (function
            | (_p1, Tcoerce_primitive _) as x ->
              x (* (p1, Tcoerce_primitive p) *)
            | p1, c1 ->
              let p2, c2 = v2.(p1) in
              (p2, compose_coercions c1 c2))
          pc1,
        ids1 @ ids2,
        runtime_fields1 )
  | Tcoerce_functor (arg1, res1), Tcoerce_functor (arg2, res2) ->
    Tcoerce_functor (compose_coercions arg2 arg1, compose_coercions res1 res2)
  | c1, Tcoerce_alias (path, c2) -> Tcoerce_alias (path, compose_coercions c1 c2)
  | _, _ -> Misc.fatal_error "Translmod.compose_coercions"

(*
let apply_coercion a b c =
  Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion b;
  apply_coercion a b c

let compose_coercions c1 c2 =
  let c3 = compose_coercions c1 c2 in
  let open Includemod in
  Format.eprintf "@[<2>compose_coercions@ (%a)@ (%a) =@ %a@]@."
    print_coercion c1 print_coercion c2 print_coercion c3;
  c3
*)

(* Record the primitive declarations occurring in the module compiled *)

let rec pure_module m : Lambda.let_kind =
  match m.mod_desc with
  | Tmod_ident _ -> Alias
  | Tmod_constraint (m, _, _, _) -> pure_module m
  | _ -> Strict

(* Generate lambda-code for a reordered list of bindings *)

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, extensions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers : Types.signature_item list -> Ident.t list =
  function
  | [] -> []
  | Sig_value (id, {val_kind = Val_reg}) :: rem ->
    id :: bound_value_identifiers rem
  | Sig_typext (id, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module (id, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_class _ :: _ -> assert false
  | _ :: rem -> bound_value_identifiers rem

(* Compile one or more functors, merging curried functors to produce
   multi-argument functors.  Any [@inline] attribute on a functor that is
   merged must be consistent with any other [@inline] attribute(s) on the
   functor(s) being merged with.  Such an attribute will be placed on the
   resulting merged functor. *)

let get_functor_params mexp coercion root_path =
  match mexp.mod_desc with
  | Tmod_functor (param, _, _, body) ->
    let inline_attribute =
      Translattribute.get_inline_attribute mexp.mod_attributes
    in
    let arg_coercion, res_coercion =
      match coercion with
      | Tcoerce_none -> (Tcoerce_none, Tcoerce_none)
      | Tcoerce_functor (arg_coercion, res_coercion) ->
        (arg_coercion, res_coercion)
      | _ -> Misc.fatal_error "Translmod.get_functor_params: bad coercion"
    in
    let loc = mexp.mod_loc in
    let path = functor_path root_path param in
    ((param, loc, arg_coercion), body, path, res_coercion, inline_attribute)
  | _ -> assert false

let export_identifiers : Ident.t list ref = ref []

let rec compile_functor mexp coercion root_path loc =
  let functor_param, body, body_path, res_coercion, inline_attribute =
    get_functor_params mexp coercion root_path
  in
  (* cf. [transl_module] *)
  let param, loc_, arg_coercion = functor_param in
  let param' = Ident.rename param in
  let arg = apply_coercion loc_ Alias arg_coercion (Lvar param') in
  let body =
    Lambda.Llet
      (Alias, Pgenval, param, arg, transl_module res_coercion body_path body)
  in
  Lambda.Lfunction
    {
      params = [param'];
      attr =
        {
          inline = inline_attribute;
          is_a_functor = true;
          return_unit = false;
          async = false;
          one_unit_arg = false;
          directive = None;
        };
      loc;
      body;
    }

(* Compile a module expression *)
and transl_module cc rootpath mexp =
  List.iter (Translattribute.check_attribute_on_module mexp) mexp.mod_attributes;
  let loc = mexp.mod_loc in
  match mexp.mod_type with
  | Mty_alias (Mta_absent, _) ->
    apply_coercion loc Alias cc Lambda.lambda_module_alias
  | _ -> (
    match mexp.mod_desc with
    | Tmod_ident (path, _) ->
      apply_coercion loc Strict cc
        (Lambda.transl_module_path ~loc mexp.mod_env path)
    | Tmod_structure str -> fst (transl_struct loc [] cc rootpath str)
    | Tmod_functor _ -> compile_functor mexp cc rootpath loc
    | Tmod_apply (funct, arg, ccarg) ->
      let inlined_attribute, funct =
        Translattribute.get_and_remove_inlined_attribute_on_module funct
      in
      apply_coercion loc Strict cc
        (Lapply
           {
             ap_loc = loc;
             ap_func = transl_module Tcoerce_none None funct;
             ap_args = [transl_module ccarg None arg];
             ap_inlined = inlined_attribute;
           })
    | Tmod_constraint (arg, _, _, ccarg) ->
      transl_module (compose_coercions cc ccarg) rootpath arg
    | Tmod_unpack (arg, _) ->
      apply_coercion loc Strict cc (Translcore.transl_exp arg))

and transl_struct loc fields cc rootpath str =
  transl_structure loc fields cc rootpath str.str_final_env str.str_items

and transl_structure loc fields cc rootpath final_env = function
  | [] -> (
    let is_top_root_path = is_top rootpath in

    match cc with
    | Tcoerce_none ->
      let block_fields =
        List.fold_left
          (fun acc id ->
            if is_top_root_path then
              export_identifiers := id :: !export_identifiers;
            Lambda.Lvar id :: acc)
          [] fields
      in
      ( Lambda.Lprim
          ( Pmakeblock
              (if is_top_root_path then Blk_module_export !export_identifiers
               else Blk_module (List.rev_map (fun id -> id.Ident.name) fields)),
            block_fields,
            loc ),
        List.length fields )
    | Tcoerce_structure (pos_cc_list, id_pos_list, runtime_fields) ->
      (* Do not ignore id_pos_list ! *)
      (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
        List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
          fields;
        Format.eprintf "@]@.";*)
      assert (List.length runtime_fields = List.length pos_cc_list);
      let v = Ext_array.reverse_of_list fields in
      let get_field pos = Lambda.Lvar v.(pos)
      and ids =
        List.fold_right Lambda.IdentSet.add fields Lambda.IdentSet.empty
      in
      let get_field_name _name = get_field in
      let result =
        List.fold_right
          (fun (pos, cc) code ->
            match cc with
            | Tcoerce_primitive p ->
              if is_top rootpath then
                export_identifiers := p.pc_id :: !export_identifiers;
              Translcore.transl_primitive p.pc_loc p.pc_desc p.pc_env p.pc_type
              :: code
            | _ ->
              if is_top rootpath then
                export_identifiers := v.(pos) :: !export_identifiers;
              apply_coercion loc Strict cc (get_field pos) :: code)
          pos_cc_list []
      in
      let lam =
        Lambda.Lprim
          ( Pmakeblock
              (if is_top_root_path then Blk_module_export !export_identifiers
               else Blk_module runtime_fields),
            result,
            loc )
      and id_pos_list =
        Ext_list.filter id_pos_list (fun (id, _, _) ->
            not (Lambda.IdentSet.mem id ids))
      in
      ( wrap_id_pos_list loc id_pos_list get_field_name lam,
        List.length pos_cc_list )
    | _ -> Misc.fatal_error "Translmod.transl_structure")
  | item :: rem -> (
    match item.str_desc with
    | Tstr_eval (expr, _) ->
      let body, size = transl_structure loc fields cc rootpath final_env rem in
      (Lsequence (Translcore.transl_exp expr, body), size)
    | Tstr_value (rec_flag, pat_expr_list) ->
      let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
      let body, size =
        transl_structure loc ext_fields cc rootpath final_env rem
      in
      (* Recursve already excludes complex pattern bindings*)
      if is_top rootpath && rec_flag = Nonrecursive then
        Ext_list.iter pat_expr_list (fun {vb_pat} ->
            match vb_pat.pat_desc with
            | Tpat_var _ | Tpat_alias _ -> ()
            | _ ->
              if not (Parmatch.irrefutable vb_pat) then
                raise (Error (vb_pat.pat_loc, Fragile_pattern_in_toplevel)));
      (Translcore.transl_let rec_flag pat_expr_list body, size)
    | Tstr_typext tyext ->
      let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
      let body, size =
        transl_structure loc
          (List.rev_append ids fields)
          cc rootpath final_env rem
      in
      (transl_type_extension item.str_env rootpath tyext body, size)
    | Tstr_exception ext ->
      let id = ext.ext_id in
      let path = field_path rootpath id in
      let body, size =
        transl_structure loc (id :: fields) cc rootpath final_env rem
      in
      ( Llet
          ( Strict,
            Pgenval,
            id,
            Translcore.transl_extension_constructor item.str_env path ext,
            body ),
        size )
    | Tstr_module mb as s ->
      let id = mb.mb_id in
      let body, size =
        transl_structure loc
          (if Typemod.rescript_hide s then fields else id :: fields)
          cc rootpath final_env rem
      in
      let module_body =
        transl_module Tcoerce_none (field_path rootpath id) mb.mb_expr
      in
      let module_body =
        Translattribute.add_inline_attribute module_body mb.mb_loc
          mb.mb_attributes
      in
      (Llet (pure_module mb.mb_expr, Pgenval, id, module_body, body), size)
    | Tstr_recmodule bindings ->
      let ext_fields =
        List.rev_append (List.map (fun mb -> mb.mb_id) bindings) fields
      in
      let body, size =
        transl_structure loc ext_fields cc rootpath final_env rem
      in
      let lam =
        Transl_recmodule.compile_recmodule
          (fun id modl _loc ->
            transl_module Tcoerce_none (field_path rootpath id) modl)
          bindings body
      in
      (lam, size)
    | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create "include" in
      let rec rebind_idents pos newfields = function
        | [] -> transl_structure loc newfields cc rootpath final_env rem
        | id :: ids ->
          let body, size = rebind_idents (pos + 1) (id :: newfields) ids in
          ( Llet
              ( Alias,
                Pgenval,
                id,
                Lprim
                  ( Pfield (pos, Fld_module {name = Ident.name id}),
                    [Lvar mid],
                    incl.incl_loc ),
                body ),
            size )
      in
      let body, size = rebind_idents 0 fields ids in
      ( Llet
          ( pure_module modl,
            Pgenval,
            mid,
            transl_module Tcoerce_none None modl,
            body ),
        size )
    | Tstr_primitive _ | Tstr_type _ | Tstr_modtype _ | Tstr_open _
    | Tstr_attribute _ ->
      transl_structure loc fields cc rootpath final_env rem)

(* Update forward declaration in Translcore *)
let _ = Translcore.transl_module := transl_module

(* Introduce dependencies on modules referenced only by "external". *)

(* Compile an implementation *)

let transl_implementation module_name (str, cc) =
  export_identifiers := [];
  let module_id = Ident.create_persistent module_name in
  let body, _ = transl_struct Location.none [] cc (global_path module_id) str in
  (body, !export_identifiers)

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

(* Compile a toplevel phrase *)

(* Error report *)

let report_error ppf = function
  | Fragile_pattern_in_toplevel ->
    Format.fprintf ppf "@[Such fragile pattern not allowed in the toplevel@]"

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
