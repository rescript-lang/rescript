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
module S = Js_stmt_make

let external_var
    ({bundle; module_bind_name; import_attributes} :
      External_ffi_types.external_module_name) =
  let id =
    Lam_compile_env.add_js_module ?import_attributes module_bind_name bundle
      false ~dynamic_import:false
  in
  E.external_var ?import_attributes ~external_name:bundle id

type arg_expression = Js_of_lam_variant.arg_expression =
  | Splice0
  | Splice1 of E.t
  | Splice2 of E.t * E.t

let append_list x xs =
  match x with
  | Splice0 -> xs
  | Splice1 a -> a :: xs
  | Splice2 (a, b) -> a :: b :: xs

(* The JS arguments that a compiled argument of an external is passed as. A
   [unit] argument, which has no label, and an [@ignore] argument are left
   out: [hold_arg] has already made their effects a statement of the
   argument. An optional argument is passed as its payload, or [undefined]
   when it is absent, which an [@int] or [@as] dispatch passes on as
   [undefined]. *)
let ocaml_to_js ~(arg_label : External_arg_spec.label_noname)
    ~(arg_type : External_arg_spec.attr) (raw_arg : E.t) : arg_expression =
  let arg =
    match arg_label with
    | Arg_optional ->
      Js_of_lam_option.get_default_undefined_from_optional raw_arg
    | Arg_label | Arg_empty -> raw_arg
  in
  match arg_type with
  | Arg_cst _ -> assert false
  (* has to be preprocessed by {!Lambda} first *)
  | Extern_unit | Ignore -> Splice0
  | Poly_var_string {descr} -> Splice1 (Js_of_lam_variant.eval arg descr)
  | Poly_var {descr} -> Js_of_lam_variant.eval_as_event arg descr
  (* FIXME: encode invariant below in the signature*)
  (* length of 2
     - the poly var tag
     - the value
  *)
  | Int dispatches -> Splice1 (Js_of_lam_variant.eval_as_int arg dispatches)
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
    Splice1 single_arg
  | Nothing -> Splice1 arg

(* Prepares a compiled argument, with the statements that run before its value
   is read, for [ocaml_to_js], so that it is evaluated once and in its place
   among the arguments (see [sequence_arguments] in [Lam_compile]).

   - The translation of an event-style polymorphic variant argument other
     than a literal reads it twice, as [e.NAME] and [e.VAL]. So unless
     reading it twice is the same as reading it once, the argument is bound
     to a temporary after its statements.
   - An [@int] or [@as] argument is passed, in its place, to a function that
     switches on it.
   - A [unit] or [@ignore] argument is not passed, so its effects become a
     statement of the argument, and its value [undefined]. Of an optional
     one, only the payload is evaluated, since its presence does not
     matter. *)
let hold_arg ~(arg_type : External_arg_spec.attr)
    ((block, arg) as compiled : J.block * E.t) : J.block * E.t =
  match arg_type with
  | Poly_var _ when not (Js_of_lam_variant.eval_as_event_in_place arg) ->
    let tmp = Ext_ident.create_tmp () in
    ( Ext_list.append_one block (S.define_variable ~kind:Strict tmp arg),
      E.var tmp )
  | Extern_unit | Ignore ->
    let effects =
      match arg.expression_desc with
      | Optional_block (x, _) -> x
      | _ -> arg
    in
    ( (if Js_analyzer.no_side_effect_expression effects then block
       else Ext_list.append_one block (S.exp effects)),
      E.unit )
  | Poly_var _ | Poly_var_string _ | Int _ | Arg_cst _ | Nothing | Unwrap ->
    compiled

(* [hold_arg] for the compiled arguments of a call, which have the
   statements that run before their values are read. The specs pair with the
   arguments as in [assemble_args_no_splice], and no spec translates an
   argument past them. The object of a [@send] call is left as it is, since
   [translate_ffi] calls the method on it whatever its spec, such as
   [@ignore], says. *)
let hold_args (decl : External_ffi_types.external_decl)
    (arg_types : External_arg_spec.params) (args : (J.block * E.t) list) =
  let rec aux (arg_types : External_arg_spec.params) args =
    match (arg_types, args) with
    | {arg_type = Arg_cst _; _} :: arg_types, args -> aux arg_types args
    | {arg_type; _} :: arg_types, arg :: args ->
      hold_arg ~arg_type arg :: aux arg_types args
    | [], args -> args
    | _ :: _, [] -> assert false
  in
  match decl.kind with
  | Decl_send _ -> (
    match (arg_types, args) with
    | _self_type :: arg_types, self :: args -> self :: aux arg_types args
    | [], _ | _, [] -> assert false)
  | Decl_val _ | Decl_new _ | Decl_get _ | Decl_set _ | Decl_get_index
  | Decl_set_index ->
    aux arg_types args

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
*)
let assemble_args_no_splice (arg_types : specs) (args : exprs) : exprs =
  let rec aux (labels : specs) (args : exprs) : exprs =
    match (labels, args) with
    | [], _ ->
      assert (args = []);
      []
    | {arg_type = Arg_cst cst; _} :: labels, args ->
      (* can not be Optional *)
      Lam_compile_const.translate_arg_cst cst :: aux labels args
    | {arg_label; arg_type} :: labels, arg :: args ->
      append_list (ocaml_to_js ~arg_label ~arg_type arg) (aux labels args)
    | _ :: _, [] -> assert false
  in
  keep_non_undefined_args arg_types (aux arg_types args)

let assemble_args_has_splice (arg_types : specs) (args : exprs) : exprs * bool =
  let dynamic = ref false in
  let rec aux (labels : specs) (args : exprs) : exprs =
    match (labels, args) with
    | [], _ ->
      assert (args = []);
      []
    | {arg_type = Arg_cst cst; _} :: labels, args ->
      Lam_compile_const.translate_arg_cst cst :: aux labels args
    | {arg_label; arg_type} :: labels, arg :: args -> (
      let accs = aux labels args in
      match (args, (arg : E.t)) with
      | [], {expression_desc = Array ls; _} -> Ext_list.append ls accs
      | _ ->
        if args = [] then dynamic := true;
        append_list (ocaml_to_js ~arg_type ~arg_label arg) accs)
    | _ :: _, [] -> assert false
  in
  let args = aux arg_types args in
  (args, !dynamic)

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
      (* A leading "default" scope is the module's default export: import
         it as such, like [external x: t = "default"] *)
      let default = x = "default" in
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

(* The function a JSX external refers to; [Translcore] only builds [Pjsx]
   for [Decl_val] externals that aren't the module itself *)
let jsx_callee
    ({kind; module_; scopes; variadic = _; effective_arity = _} :
      External_ffi_types.external_decl) =
  match (kind, module_) with
  | Decl_val {name}, None -> translate_scoped_module_val None name scopes
  | Decl_val {name}, Some (Module_named emn) ->
    translate_scoped_module_val (Some emn) name scopes
  | Decl_val _, Some Module_itself
  | ( ( Decl_send _ | Decl_new _ | Decl_get _ | Decl_set _ | Decl_get_index
      | Decl_set_index ),
      _ ) ->
    assert false

let translate_scoped_access scopes obj =
  match scopes with
  | [] -> obj
  | x :: xs -> Ext_list.fold_left xs (E.dot obj x) E.dot

let translate_ffi (cxt : Lam_compile_context.t) arg_types ~(prim_name : string)
    (decl : External_ffi_types.external_decl) (args : J.expression list) =
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
  (* the JS arguments of a call, spread when [@variadic] needs it *)
  let assemble_args arg_types args =
    if splice then
      let args, dynamic = assemble_args_has_splice arg_types args in
      if dynamic then E.variadic_args args else args
    else assemble_args_no_splice arg_types args
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
    E.call ~info:Js_call_info.na_full_call fn (assemble_args arg_types args)
  | Decl_new _, Some Module_itself ->
    (* the module itself, as a class *)
    let fn = external_var (module_itself_name ()) in
    let args = assemble_args_no_splice arg_types args in
    (* TODO: fix in rest calling convention *)
    mark_new_object ();
    E.new_ fn args
  | ( (Decl_send _ | Decl_get _ | Decl_set _ | Decl_get_index | Decl_set_index),
      Some Module_itself ) ->
    assert false (* rejected during digestion *)
  | Decl_val {name}, _ when decl.effective_arity = 0 -> (
    (* a global value. A call applies it to the arguments of a function type
       that the external does not spell out, or to the [@ignore] arguments
       of one that it does, which are not passed. *)
    let e = translate_scoped_module_val named_module name scopes in
    match (arg_types, args) with
    | _, [] -> e
    | [], _ -> E.call ~info:Js_call_info.na_full_call e args
    | _ :: _, _ ->
      E.call ~info:Js_call_info.na_full_call e
        (assemble_args_no_splice arg_types args))
  | Decl_val {name = fn}, _ ->
    (* a global (possibly module-scoped) function call *)
    let fn = translate_scoped_module_val named_module fn scopes in
    E.call ~info:Js_call_info.na_full_call fn (assemble_args arg_types args)
  | Decl_new {name = fn}, _ ->
    let args = assemble_args arg_types args in
    let fn = translate_scoped_module_val named_module fn scopes in
    mark_new_object ();
    E.new_ fn args
  | Decl_send {name}, _ -> (
    match args with
    | self :: args ->
      (* PR2162 [self_type] more checks in syntax:
         - should not be [@as] *)
      let arg_types =
        match arg_types with
        | _self_type :: arg_types -> arg_types
        | [] -> assert false
      in
      let args = assemble_args arg_types args in
      let self = translate_scoped_access scopes self in
      E.call ~info:Js_call_info.na_full_call (E.dot self name) args
    | _ -> assert false)
  | Decl_get {name}, _ -> (
    match assemble_args_no_splice arg_types args with
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
    match assemble_args_no_splice arg_types args with
    | [obj; v] ->
      let obj = translate_scoped_access scopes obj in
      E.assign (E.dot obj name) v
    | _ -> assert false)
  | Decl_get_index, _ -> (
    match assemble_args_no_splice arg_types args with
    | [obj; v] ->
      Js_of_lam_array.ref_array (translate_scoped_access scopes obj) v
    | _ -> assert false)
  | Decl_set_index, _ -> (
    match assemble_args_no_splice arg_types args with
    | [obj; v; value] ->
      Js_of_lam_array.set_array (translate_scoped_access scopes obj) v value
    | _ -> assert false)
