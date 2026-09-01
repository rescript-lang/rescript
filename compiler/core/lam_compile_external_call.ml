(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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

[@@@warning "+9"]

module E = Js_exp_make

(** 
   [bind_name] is a hint to the compiler to generate 
   better names for external module 
*)
(* let handle_external
    ({bundle ; module_bind_name} : External_ffi_types.external_module_name)
   : Ident.t * string
   =
   Lam_compile_env.add_js_module module_bind_name bundle ,
   bundle *)

let external_var
    ({bundle; module_bind_name; import_attributes} :
      External_ffi_types.external_module_name) =
  let id =
    Lam_compile_env.add_js_module ?import_attributes module_bind_name bundle
      false ~dynamic_import:false
  in
  E.external_var ?import_attributes ~external_name:bundle id

(* let handle_external_opt
    (module_name : External_ffi_types.external_module_name option)
   : (Ident.t * string) option =
   match module_name with
   | Some module_name -> Some (handle_external module_name)
   | None -> None
*)

type arg_expression = Js_of_lam_variant.arg_expression =
  | Splice0
  | Splice1 of E.t
  | Splice2 of E.t * E.t

let append_list x xs =
  match x with
  | Splice0 -> xs
  | Splice1 a -> a :: xs
  | Splice2 (a, b) -> a :: b :: xs

(* The first return value is value, the second argument is side effect expressions
    Only the [unit] with no label will be ignored
    When  we are passing a boxed value to external(optional), we need
    unbox it in the first place.

    Note when optional value is not passed, the unboxed value would be
    [undefined], with the combination of `[@int]` it would be still be
    [undefined], this by default is still correct..
   {[
     (function () {
          switch (undefined) {
            case 97 :
              return "a";
            case 98 :
              return "b";

          }
        }()) === undefined
   ]}

     This would not work with [NonNullString]
*)
let ocaml_to_js_eff ~(arg_label : External_arg_spec.label_noname)
    ~(arg_type : External_arg_spec.attr) (raw_arg : E.t) :
    arg_expression * E.t list =
  let arg =
    match arg_label with
    | Arg_optional ->
      Js_of_lam_option.get_default_undefined_from_optional raw_arg
    | Arg_label | Arg_empty -> raw_arg
  in
  match arg_type with
  | Arg_cst _ -> assert false
  (* has to be preprocessed by {!Lambda} first *)
  | Extern_unit ->
    ( (if arg_label = Arg_empty then Splice0 else Splice1 E.unit),
      if Js_analyzer.no_side_effect_expression arg then [] else [arg] )
    (* leave up later to decide *)
  | Ignore ->
    (Splice0, if Js_analyzer.no_side_effect_expression arg then [] else [arg])
  | Poly_var_string {descr} -> (Splice1 (Js_of_lam_variant.eval arg descr), [])
  | Poly_var {descr} -> (Js_of_lam_variant.eval_as_event arg descr, [])
  (* FIXME: encode invariant below in the signature*)
  (* length of 2
     - the poly var tag
     - the value
  *)
  | Int dispatches ->
    (Splice1 (Js_of_lam_variant.eval_as_int arg dispatches), [])
  | Unwrap ->
    let single_arg =
      match arg_label with
      | Arg_optional ->
        (*
           If this is an optional arg (like `?arg`), we have to potentially do
           2 levels of unwrapping:
           - if ocaml arg is `None`, let js arg be `undefined` (no unwrapping)
           - if ocaml arg is `Some x`, unwrap the arg to get the `x`, then
             unwrap the `x` itself
           - Here `Some x` is `x` due to the current encoding
             Lets inline here since it depends on the runtime encoding
        *)
        Js_of_lam_option.option_unwrap raw_arg
      | _ -> Js_of_lam_variant.eval_as_unwrap raw_arg
    in
    (Splice1 single_arg, [])
  | Nothing -> (Splice1 arg, [])

let empty_pair = ([], [])

let add_eff eff e =
  match eff with
  | None -> e
  | Some v -> E.seq v e

type specs = External_arg_spec.params

type exprs = E.t list

let keep_non_undefined_args (arg_types : specs) (args : exprs) =
  let rec has_undefined_trailing_args arg_types args =
    match (arg_types, args) with
    | ( [{External_arg_spec.arg_label = Arg_optional; _}],
        [{J.expression_desc = Undefined {is_unit = false}; _}] ) ->
      true
    | _ :: arg_types_rest, _ :: args_rest ->
      has_undefined_trailing_args arg_types_rest args_rest
    | _ -> false
  in
  let rec aux arg_types args =
    match (arg_types, args) with
    | ( {External_arg_spec.arg_label = Arg_optional; _} :: arg_types_rest,
        {J.expression_desc = Undefined {is_unit = false}; _} :: args_rest ) ->
      aux arg_types_rest args_rest
    | _ -> args
  in
  if has_undefined_trailing_args arg_types args then
    aux (List.rev arg_types) (List.rev args) |> List.rev
  else args

(* TODO: fix splice,
   we need a static guarantee that it is static array construct
   otherwise, we should provide a good error message here,
   no compiler failure here
   Invariant : Array encoding
   @return arguments and effect
*)
let assemble_args_no_splice (arg_types : specs) (args : exprs) :
    exprs * E.t option =
  let rec aux (labels : specs) (args : exprs) : exprs * exprs =
    match (labels, args) with
    | [], _ ->
      assert (args = []);
      empty_pair
    | {arg_type = Arg_cst cst; _} :: labels, args ->
      (* can not be Optional *)
      let accs, eff = aux labels args in
      (Lam_compile_const.translate_arg_cst cst :: accs, eff)
    | {arg_label; arg_type} :: labels, arg :: args ->
      let accs, eff = aux labels args in
      let acc, new_eff = ocaml_to_js_eff ~arg_label ~arg_type arg in
      (append_list acc accs, Ext_list.append new_eff eff)
    | _ :: _, [] -> assert false
  in
  let args, eff = aux arg_types args in
  ( keep_non_undefined_args arg_types args,
    match eff with
    | [] -> None
    | x :: xs ->
      (* FIXME: the order of effects? *)
      Some (E.fuse_to_seq x xs) )

let assemble_args_has_splice (arg_types : specs) (args : exprs) :
    exprs * E.t option * bool =
  let dynamic = ref false in
  let rec aux (labels : specs) (args : exprs) =
    match (labels, args) with
    | [], _ ->
      assert (args = []);
      empty_pair
    | {arg_type = Arg_cst cst; _} :: labels, args ->
      let accs, eff = aux labels args in
      (Lam_compile_const.translate_arg_cst cst :: accs, eff)
    | {arg_label; arg_type} :: labels, arg :: args -> (
      let accs, eff = aux labels args in
      match (args, (arg : E.t)) with
      | [], {expression_desc = Array ls; _} -> (Ext_list.append ls accs, eff)
      | _ ->
        if args = [] then dynamic := true;
        let acc, new_eff = ocaml_to_js_eff ~arg_type ~arg_label arg in
        (append_list acc accs, Ext_list.append new_eff eff))
    | _ :: _, [] -> assert false
  in
  let args, eff = aux arg_types args in
  ( args,
    (match eff with
    | [] -> None
    | x :: xs ->
      (* FIXME: the order of effects? *)
      Some (E.fuse_to_seq x xs)),
    !dynamic )

let translate_scoped_module_val
    (module_name : External_ffi_types.external_module_name option) (fn : string)
    (scopes : string list) =
  match module_name with
  | Some {bundle; module_bind_name; import_attributes} -> (
    match scopes with
    | [] ->
      let default = fn = "default" in
      let id =
        Lam_compile_env.add_js_module ?import_attributes module_bind_name bundle
          default ~dynamic_import:false
      in
      E.external_var_field ?import_attributes ~external_name:bundle ~field:fn
        ~default id
    | x :: rest ->
      (* TODO: what happens when scope contains "default" ?*)
      let default = false in
      let id =
        Lam_compile_env.add_js_module ?import_attributes module_bind_name bundle
          default ~dynamic_import:false
      in
      let start =
        E.external_var_field ?import_attributes ~external_name:bundle ~field:x
          ~default id
      in
      Ext_list.fold_left (Ext_list.append rest [fn]) start E.dot)
  | None -> (
    (*  no [@@module], assume it's global *)
    match scopes with
    | [] -> E.js_global fn
    | x :: rest ->
      let start = E.js_global x in
      Ext_list.fold_left (Ext_list.append_one rest fn) start E.dot)

let translate_scoped_access scopes obj =
  match scopes with
  | [] -> obj
  | x :: xs -> Ext_list.fold_left xs (E.dot obj x) E.dot

let translate_ffi ?(transformed_jsx = false) (cxt : Lam_compile_context.t)
    arg_types ~(prim_name : string) (decl : External_ffi_types.external_decl)
    (args : J.expression list) =
  let {External_ffi_types.kind; module_; scopes; variadic = splice; _} = decl in
  (* a bare [@module] binds the module itself under the primitive name *)
  let module_itself_name () : External_ffi_types.external_module_name =
    {
      bundle = prim_name;
      module_bind_name = Phint_nothing;
      import_attributes = None;
    }
  in
  let named_module : External_ffi_types.external_module_name option =
    match module_ with
    | Some (Module_named emn) -> Some emn
    | Some Module_itself | None -> None
  in
  let mark_new_object () =
    (* handle [@@new]: mark the bound identifier (if any) as an object;
       order matters, since the property affects later code generation *)
    match cxt.continuation with
    | Declare (_, id) | Assign id -> Ext_ident.make_js_object id
    | EffectCall _ | NeedValue _ -> ()
  in
  match (kind, module_) with
  | Decl_val _, Some Module_itself when decl.effective_arity = 0 ->
    (* the module itself, as a value *)
    external_var (module_itself_name ())
  | Decl_val _, Some Module_itself ->
    (* the module itself, called as a function *)
    let fn = external_var (module_itself_name ()) in
    if splice then
      let args, eff, dynamic = assemble_args_has_splice arg_types args in
      let args = if dynamic then E.variadic_args args else args in
      add_eff eff
        (E.call ~info:(Js_call_info.na_full_call transformed_jsx) fn args)
    else
      let args, eff = assemble_args_no_splice arg_types args in
      (* TODO: fix in rest calling convention *)
      add_eff eff
        (E.call ~info:(Js_call_info.na_full_call transformed_jsx) fn args)
  | Decl_new _, Some Module_itself ->
    (* the module itself, as a class *)
    let fn = external_var (module_itself_name ()) in
    let args, eff = assemble_args_no_splice arg_types args in
    (* TODO: fix in rest calling convention *)
    add_eff eff
      (mark_new_object ();
       E.new_ fn args)
  | ( (Decl_send _ | Decl_get _ | Decl_set _ | Decl_get_index | Decl_set_index),
      Some Module_itself ) ->
    assert false (* rejected during digestion *)
  | Decl_val {name}, _ when decl.effective_arity = 0 ->
    (* a global value *)
    let e = translate_scoped_module_val named_module name scopes in
    if args = [] then e
    else E.call ~info:(Js_call_info.na_full_call transformed_jsx) e args
  | Decl_val {name = fn}, _ ->
    (* a global (possibly module-scoped) function call *)
    let fn = translate_scoped_module_val named_module fn scopes in
    if splice then
      let args, eff, dynamic = assemble_args_has_splice arg_types args in
      let args = if dynamic then E.variadic_args args else args in
      add_eff eff
        (E.call ~info:(Js_call_info.na_full_call transformed_jsx) fn args)
    else
      let args, eff = assemble_args_no_splice arg_types args in
      add_eff eff
      @@ E.call
           ~info:{call_info = Call_na; call_transformed_jsx = transformed_jsx}
           fn args
  | Decl_new {name = fn}, _ ->
    if splice then
      let args, eff, dynamic = assemble_args_has_splice arg_types args in
      let args = if dynamic then E.variadic_args args else args in
      let fn = translate_scoped_module_val named_module fn scopes in
      add_eff eff
        (mark_new_object ();
         E.new_ fn args)
    else
      let args, eff = assemble_args_no_splice arg_types args in
      let fn = translate_scoped_module_val named_module fn scopes in
      add_eff eff
        (mark_new_object ();
         E.new_ fn args)
  | Decl_send {name}, _ -> (
    match args with
    | self :: args ->
      (* PR2162 [self_type] more checks in syntax:
         - should not be [@as] *)
      let[@warning "-8"] (_self_type :: arg_types) = arg_types in
      if splice then
        let args, eff, dynamic = assemble_args_has_splice arg_types args in
        let args = if dynamic then E.variadic_args args else args in
        add_eff eff
          (let self = translate_scoped_access scopes self in
           E.call
             ~info:{call_info = Call_na; call_transformed_jsx = transformed_jsx}
             (E.dot self name) args)
      else
        let args, eff = assemble_args_no_splice arg_types args in
        add_eff eff
          (let self = translate_scoped_access scopes self in
           E.call
             ~info:(Js_call_info.na_full_call transformed_jsx)
             (E.dot self name) args)
    | _ -> assert false)
  | Decl_get {name}, _ -> (
    let args, cur_eff = assemble_args_no_splice arg_types args in
    add_eff cur_eff
    @@
    match args with
    | [obj] ->
      let obj = translate_scoped_access scopes obj in
      E.dot obj name
    | _ ->
      (* This should have been caught in the frontend validation. *)
      invalid_arg
        "Internal compiler error: @get external called with wrong number of \
         arguments. Expected exactly one object argument."
    (* Note these assertion happens in call site *))
  | Decl_set {name}, _ -> (
    (* assert (js_splice = false) ;  *)
    let args, cur_eff = assemble_args_no_splice arg_types args in
    add_eff cur_eff
    @@
    match (args, arg_types) with
    | [obj; v], _ ->
      let obj = translate_scoped_access scopes obj in
      E.assign (E.dot obj name) v
    | _ -> assert false)
  | Decl_get_index, _ -> (
    let args, cur_eff = assemble_args_no_splice arg_types args in
    add_eff cur_eff
    @@
    match args with
    | [obj; v] ->
      Js_of_lam_array.ref_array (translate_scoped_access scopes obj) v
    | _ -> assert false)
  | Decl_set_index, _ -> (
    let args, cur_eff = assemble_args_no_splice arg_types args in
    add_eff cur_eff
    @@
    match args with
    | [obj; v; value] ->
      Js_of_lam_array.set_array (translate_scoped_access scopes obj) v value
    | _ -> assert false)
