(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
 * 
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

let caml_id_field_info : Lambda.field_dbg_info =
  Fld_record {name = Literals.exception_id; mutable_flag = Immutable}

let lam_caml_id : Lam_primitive.t = Pfield (0, caml_id_field_info)
let prim = Lam.prim

let lam_extension_id loc (head : Lam.t) =
  prim ~primitive:lam_caml_id ~args:[head] loc

let lazy_block_info : Lam_tag_info.t =
  Blk_record
    {
      fields = [|(Literals.lazy_done, false); (Literals.lazy_val, false)|];
      mutable_flag = Mutable;
    }

(** A conservative approach to avoid packing exceptions
    for lambda expression like {[
      try { ... }catch(id){body}
    ]}
    we approximate that if [id] is destructed or not.
    If it is destructed, we need pack it in case it is JS exception.
    The packing is called Js.Exn.internalTOOCamlException, which is a nop for OCaml exception, 
    but will wrap as (Error e) when it is an JS exception. 

    {[
      try .. with 
      | A (x,y) -> 
      | Js.Error ..
    ]}

    Without such wrapping, the code above would raise

    Note it is not guaranteed that exception raised(or re-raised) is a structured
    ocaml exception but it is guaranteed that if such exception is processed it would
    still be an ocaml exception.
    for example {[
      match x with
      | exception e -> raise e
    ]}
    it will re-raise an exception as it is (we are not packing it anywhere)

    It is hard to judge an exception is destructed or escaped, any potential
    alias(or if it is passed as an argument) would cause it to be leaked
*)
let exception_id_destructed (l : Lam.t) (fv : Ident.t) : bool =
  let rec hit_opt (x : _ option) =
    match x with
    | None -> false
    | Some a -> hit a
  and hit_list_snd : 'a. ('a * _) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : Lam.t) =
    match l with
    (* | Lprim {primitive = Pintcomp _ ;
             args = ([x;y ])  } ->
       begin match x,y with
        | Lvar _, Lvar _ -> false
        | Lvar _, _ -> hit y
        | _, Lvar _ -> hit x
        | _, _  -> hit x || hit y
       end *)
    (* FIXME: this can be uncovered after we do the unboxing *)
    | Lprim {primitive = Praise; args = [Lvar _]} -> false
    | Lprim {primitive = _; args; _} -> hit_list args
    | Lvar id -> Ident.same id fv
    | Lassign (id, e) -> Ident.same id fv || hit e
    | Lstaticcatch (e1, (_, _vars), e2) -> hit e1 || hit e2
    | Ltrywith (e1, _exn, e2) -> hit e1 || hit e2
    | Lfunction {body; params = _} -> hit body
    | Llet (_str, _id, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_v, e1, e2, _dir, e3) -> hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply {ap_func; ap_args; _} -> hit ap_func || hit_list ap_args
    | Lglobal_module _ (* global persistent module, play safe *) -> false
    | Lswitch (arg, sw) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default) ->
      hit arg || hit_list_snd cases || hit_opt default
    | Lstaticraise (_, args) -> hit_list args
    | Lifthenelse (e1, e2, e3) -> hit e1 || hit e2 || hit e3
    | Lsequence (e1, e2) -> hit e1 || hit e2
    | Lwhile (e1, e2) -> hit e1 || hit e2
  in
  hit l

let abs_int x = if x < 0 then -x else x
let no_over_flow x = abs_int x < 0x1fff_ffff

let lam_is_var (x : Lam.t) (y : Ident.t) =
  match x with
  | Lvar y2 -> Ident.same y2 y
  | _ -> false

(** Make sure no int range overflow happens
    also we only check [int]
*)
let happens_to_be_diff (sw_consts : (int * Lambda.lambda) list) sw_names :
    int option =
  match sw_consts with
  | (a, Lconst (Const_base (Const_int a0)))
    :: (b, Lconst (Const_base (Const_int b0)))
    :: rest
    when sw_names = None && no_over_flow a && no_over_flow a0 && no_over_flow b
         && no_over_flow b0 ->
    let diff = a0 - a in
    if b0 - b = diff then
      if
        Ext_list.for_all rest (fun (x, lam) ->
            match lam with
            | Lconst (Const_base (Const_int x0))
              when no_over_flow x0 && no_over_flow x ->
              x0 - x = diff
            | _ -> false)
      then Some diff
      else None
    else None
  | _ -> None

(* type required_modules = Lam_module_ident.Hash_set.t *)

(** drop Lseq (List! ) etc 
    see #3852, we drop all these required global modules
    but added it back based on our own module analysis
*)
let seq = Lam.seq

let unit = Lam.unit

let lam_prim ~primitive:(p : Lambda.primitive) ~args loc : Lam.t =
  match p with
  | Pidentity -> Ext_list.singleton_exn args
  | Pnull -> Lam.const Const_js_null
  | Pundefined -> Lam.const (Const_js_undefined {is_unit = false})
  | Pccall _ -> assert false
  | Prevapply -> assert false
  | Pdirapply -> assert false
  | Ploc _ -> assert false (* already compiled away here*)
  | Pcreate_extension s -> prim ~primitive:(Pcreate_extension s) ~args loc
  | Pextension_slot_eq -> (
    match args with
    | [lhs; rhs] ->
      prim ~primitive:(Pstringcomp Ceq)
        ~args:[lam_extension_id loc lhs; rhs]
        loc
    | _ -> assert false)
  | Pwrap_exn -> prim ~primitive:Pwrap_exn ~args loc
  | Pignore ->
    (* Pignore means return unit, it is not an nop *)
    seq (Ext_list.singleton_exn args) unit
  | Pgetglobal _ -> assert false
  | Pmakeblock info -> (
    let tag = Lambda.tag_of_tag_info info in
    let mutable_flag = Lambda.mutable_flag_of_tag_info info in
    match info with
    | Blk_some_not_nested -> prim ~primitive:Psome_not_nest ~args loc
    | Blk_some -> prim ~primitive:Psome ~args loc
    | Blk_constructor _ | Blk_tuple | Blk_record _ | Blk_record_inlined _
    | Blk_module _ | Blk_module_export _ | Blk_extension | Blk_record_ext _ ->
      prim ~primitive:(Pmakeblock (tag, info, mutable_flag)) ~args loc
    | Blk_poly_var s -> (
      match args with
      | [_; value] ->
        let tag_val : Lam_constant.t =
          if Ext_string.is_valid_hash_number s then
            Const_int {i = Ext_string.hash_number_as_i32_exn s; comment = None}
          else Const_string {s; unicode = false}
        in
        prim
          ~primitive:(Pmakeblock (tag, info, mutable_flag))
          ~args:[Lam.const tag_val; value]
          loc
      | _ -> assert false)
    | Blk_lazy_general -> (
      match args with
      | [((Lvar _ | Lconst _ | Lfunction _) as result)] ->
        let args = [Lam.const Const_js_true; result] in
        prim ~primitive:(Pmakeblock (tag, lazy_block_info, Mutable)) ~args loc
      | [computation] ->
        let args =
          [
            Lam.const Const_js_false;
            (* FIXME: arity 0 does not get proper supported*)
            Lam.function_ ~arity:0 ~params:[] ~body:computation
              ~attr:Lambda.default_function_attribute;
          ]
        in
        prim ~primitive:(Pmakeblock (tag, lazy_block_info, Mutable)) ~args loc
      | _ -> assert false))
  | Pfn_arity -> prim ~primitive:Pfn_arity ~args loc
  | Pdebugger -> prim ~primitive:Pdebugger ~args loc
  | Ptypeof -> prim ~primitive:Ptypeof ~args loc
  | Pisnullable -> prim ~primitive:Pis_null_undefined ~args loc
  | Pnull_to_opt -> prim ~primitive:Pnull_to_opt ~args loc
  | Pnullable_to_opt -> prim ~primitive:Pnull_undefined_to_opt ~args loc
  | Pundefined_to_opt -> prim ~primitive:Pundefined_to_opt ~args loc
  | Pis_not_none -> prim ~primitive:Pis_not_none ~args loc
  | Pval_from_option -> prim ~primitive:Pval_from_option ~args loc
  | Pval_from_option_not_nest ->
    prim ~primitive:Pval_from_option_not_nest ~args loc
  | Pjscomp x -> prim ~primitive:(Pjscomp x) ~args loc
  | Pfield (id, info) -> prim ~primitive:(Pfield (id, info)) ~args loc
  | Psetfield (id, info) -> prim ~primitive:(Psetfield (id, info)) ~args loc
  | Pduprecord -> prim ~primitive:Pduprecord ~args loc
  | Praise _ -> prim ~primitive:Praise ~args loc
  | Pobjcomp x -> prim ~primitive:(Pobjcomp x) ~args loc
  | Pobjorder -> prim ~primitive:Pobjorder ~args loc
  | Pobjmin -> prim ~primitive:Pobjmin ~args loc
  | Pobjmax -> prim ~primitive:Pobjmax ~args loc
  | Pobjtag -> prim ~primitive:Pobjtag ~args loc
  | Pobjsize -> prim ~primitive:Pobjsize ~args loc
  | Psequand -> prim ~primitive:Psequand ~args loc
  | Psequor -> prim ~primitive:Psequor ~args loc
  | Pnot -> prim ~primitive:Pnot ~args loc
  | Pboolcomp x -> prim ~primitive:(Pboolcomp x) ~args loc
  | Pboolorder -> prim ~primitive:Pboolorder ~args loc
  | Pboolmin -> prim ~primitive:Pboolmin ~args loc
  | Pboolmax -> prim ~primitive:Pboolmax ~args loc
  | Pnegint -> prim ~primitive:Pnegint ~args loc
  | Paddint -> prim ~primitive:Paddint ~args loc
  | Psubint -> prim ~primitive:Psubint ~args loc
  | Pmulint -> prim ~primitive:Pmulint ~args loc
  | Pdivint -> prim ~primitive:Pdivint ~args loc
  | Pmodint -> prim ~primitive:Pmodint ~args loc
  | Pandint -> prim ~primitive:Pandint ~args loc
  | Porint -> prim ~primitive:Porint ~args loc
  | Pxorint -> prim ~primitive:Pxorint ~args loc
  | Plslint -> prim ~primitive:Plslint ~args loc
  | Plsrint -> prim ~primitive:Plsrint ~args loc
  | Pasrint -> prim ~primitive:Pasrint ~args loc
  | Pintorder -> prim ~primitive:Pintorder ~args loc
  | Pintmin -> prim ~primitive:Pintmin ~args loc
  | Pintmax -> prim ~primitive:Pintmax ~args loc
  | Pstringlength -> prim ~primitive:Pstringlength ~args loc
  | Pstringrefu -> prim ~primitive:Pstringrefu ~args loc
  | Pstringcomp x -> prim ~primitive:(Pstringcomp x) ~args loc
  | Pstringorder -> prim ~primitive:Pstringorder ~args loc
  | Pstringmin -> prim ~primitive:Pstringmin ~args loc
  | Pstringmax -> prim ~primitive:Pstringmax ~args loc
  | Pstringadd -> prim ~primitive:Pstringadd ~args loc
  | Pabsfloat -> assert false
  | Pstringrefs -> prim ~primitive:Pstringrefs ~args loc
  | Pisint -> prim ~primitive:Pisint ~args loc
  | Pisout -> (
    match args with
    | [range; Lprim {primitive = Poffsetint i; args = [x]}] ->
      prim ~primitive:(Pisout i) ~args:[range; x] loc
    | _ -> prim ~primitive:(Pisout 0) ~args loc)
  | Pintoffloat -> prim ~primitive:Pintoffloat ~args loc
  | Pfloatofint -> prim ~primitive:Pfloatofint ~args loc
  | Pnegfloat -> prim ~primitive:Pnegfloat ~args loc
  | Paddfloat -> prim ~primitive:Paddfloat ~args loc
  | Psubfloat -> prim ~primitive:Psubfloat ~args loc
  | Pmulfloat -> prim ~primitive:Pmulfloat ~args loc
  | Pdivfloat -> prim ~primitive:Pdivfloat ~args loc
  | Pmodfloat -> prim ~primitive:Pmodfloat ~args loc
  | Pfloatorder -> prim ~primitive:Pfloatorder ~args loc
  | Pfloatmin -> prim ~primitive:Pfloatmin ~args loc
  | Pfloatmax -> prim ~primitive:Pfloatmax ~args loc
  | Pnegbigint -> prim ~primitive:Pnegbigint ~args loc
  | Paddbigint -> prim ~primitive:Paddbigint ~args loc
  | Psubbigint -> prim ~primitive:Psubbigint ~args loc
  | Pmulbigint -> prim ~primitive:Pmulbigint ~args loc
  | Pdivbigint -> prim ~primitive:Pdivbigint ~args loc
  | Pmodbigint -> prim ~primitive:Pmodbigint ~args loc
  | Ppowbigint -> prim ~primitive:Ppowbigint ~args loc
  | Pandbigint -> prim ~primitive:Pandbigint ~args loc
  | Porbigint -> prim ~primitive:Porbigint ~args loc
  | Pxorbigint -> prim ~primitive:Pxorbigint ~args loc
  | Plslbigint -> prim ~primitive:Plslbigint ~args loc
  | Pasrbigint -> prim ~primitive:Pasrbigint ~args loc
  | Pbigintcomp x -> prim ~primitive:(Pbigintcomp x) ~args loc
  | Pbigintorder -> prim ~primitive:Pbigintorder ~args loc
  | Pbigintmin -> prim ~primitive:Pbigintmin ~args loc
  | Pbigintmax -> prim ~primitive:Pbigintmax ~args loc
  | Pintcomp x -> prim ~primitive:(Pintcomp x) ~args loc
  | Poffsetint x -> prim ~primitive:(Poffsetint x) ~args loc
  | Poffsetref x -> prim ~primitive:(Poffsetref x) ~args loc
  | Pfloatcomp x -> prim ~primitive:(Pfloatcomp x) ~args loc
  | Pmakearray _mutable_flag (*FIXME*) -> prim ~primitive:Pmakearray ~args loc
  | Parraylength -> prim ~primitive:Parraylength ~args loc
  | Parrayrefu -> prim ~primitive:Parrayrefu ~args loc
  | Parraysetu -> prim ~primitive:Parraysetu ~args loc
  | Parrayrefs -> prim ~primitive:Parrayrefs ~args loc
  | Parraysets -> prim ~primitive:Parraysets ~args loc
  | Pmakelist _mutable_flag (*FIXME*) -> prim ~primitive:Pmakelist ~args loc
  | Pmakedict -> prim ~primitive:Pmakedict ~args loc
  | Pawait -> prim ~primitive:Pawait ~args loc
  | Pimport -> prim ~primitive:Pimport ~args loc
  | Pinit_mod -> (
    match args with
    | [_loc; Lconst (Const_block (0, _, [Const_block (0, _, [])]))] -> Lam.unit
    | _ -> prim ~primitive:Pinit_mod ~args loc)
  | Pupdate_mod -> (
    match args with
    | [Lconst (Const_block (0, _, [Const_block (0, _, [])])); _; _] -> Lam.unit
    | _ -> prim ~primitive:Pupdate_mod ~args loc)
  | Phash -> prim ~primitive:Phash ~args loc
  | Phash_mixint -> prim ~primitive:Phash_mixint ~args loc
  | Phash_mixstring -> prim ~primitive:Phash_mixstring ~args loc
  | Phash_finalmix -> prim ~primitive:Phash_finalmix ~args loc
  | Pcurry_apply _ -> prim ~primitive:Pjs_apply ~args loc
  | Pis_poly_var_block -> prim ~primitive:Pis_poly_var_block ~args loc
  | Pjs_raw_expr -> assert false
  | Pjs_raw_stmt -> assert false
  | Pjs_fn_make arity -> prim ~primitive:(Pjs_fn_make arity) ~args loc
  | Pjs_fn_make_unit -> prim ~primitive:Pjs_fn_make_unit ~args loc
  | Pjs_fn_method -> prim ~primitive:Pjs_fn_method ~args loc

(* Does not exist since we compile array in js backend unlike native backend *)

let may_depend = Lam_module_ident.Hash_set.add

let rec rename_optional_parameters map params (body : Lambda.lambda) =
  match body with
  | Llet
      ( k,
        value_kind,
        id,
        Lifthenelse
          ( Lprim (p, [Lvar ({name = "*opt*"} as opt)], p_loc),
            Lprim (p1, [Lvar ({name = "*opt*"} as opt2)], x_loc),
            f ),
        rest )
    when Ident.same opt opt2 && List.mem opt params ->
    let map, rest = rename_optional_parameters map params rest in
    let new_id = Ident.create (id.name ^ "Opt") in
    ( Map_ident.add map opt new_id,
      Lambda.Llet
        ( k,
          value_kind,
          id,
          Lifthenelse
            ( Lprim (p, [Lvar new_id], p_loc),
              Lprim (p1, [Lvar new_id], x_loc),
              f ),
          rest ) )
  | _ -> (map, body)

let convert (exports : Set_ident.t) (lam : Lambda.lambda) :
    Lam.t * Lam_module_ident.Hash_set.t =
  let alias_tbl = Hash_ident.create 64 in
  let exit_map = Hash_int.create 0 in
  let may_depends = Lam_module_ident.Hash_set.create 0 in

  let rec convert_ccall (a_prim : Primitive.description)
      (args : Lambda.lambda list) loc ~dynamic_import : Lam.t =
    let prim_name = a_prim.prim_name in
    match External_ffi_types.from_string a_prim.prim_native_name with
    | Ffi_obj_create labels ->
      let args = Ext_list.map args convert_aux in
      prim ~primitive:(Pjs_object_create labels) ~args loc
    | Ffi_bs (arg_types, result_type, ffi) ->
      let arg_types =
        match arg_types with
        | Params ls -> ls
        | Param_number i -> Ext_list.init i (fun _ -> External_arg_spec.dummy)
      in
      let args = Ext_list.map args convert_aux in
      Lam.handle_bs_non_obj_ffi arg_types result_type ffi args loc prim_name
        ~dynamic_import
    | Ffi_inline_const i -> Lam.const i
    | Ffi_normal ->
      Location.raise_errorf ~loc
        "@{<error>Error:@} internal error, using unrecognized primitive %s"
        prim_name
  and convert_aux ?(dynamic_import = false) (lam : Lambda.lambda) : Lam.t =
    match lam with
    | Lvar x -> Lam.var (Hash_ident.find_default alias_tbl x x)
    | Lconst x -> Lam.const (Lam_constant_convert.convert_constant x)
    | Lapply {ap_func = Lsend (name, obj, loc); ap_args}
      when Ext_string.ends_with name Literals.setter_suffix ->
      let obj = convert_aux obj in
      let args = obj :: Ext_list.map ap_args convert_aux in
      let property =
        String.sub name 0 (String.length name - Literals.setter_suffix_len)
      in
      prim
        ~primitive:(Pjs_unsafe_downgrade {name = property; setter = true})
        ~args loc
    | Lsend (name, obj, loc) ->
      let obj = convert_aux obj in
      let args = [obj] in
      let setter = Ext_string.ends_with name Literals.setter_suffix in
      let _ = assert (not setter) in
      prim ~primitive:(Pjs_unsafe_downgrade {name; setter}) ~args loc
    | Lapply {ap_func = fn; ap_args = args; ap_loc = loc; ap_inlined} ->
      (* we need do this eargly in case [aux fn] add some wrapper *)
      Lam.apply (convert_aux fn)
        (Ext_list.map args convert_aux)
        {ap_loc = loc; ap_inlined; ap_status = App_uncurry}
    | Lfunction {params; body; attr} ->
      let new_map, body =
        rename_optional_parameters Map_ident.empty params body
      in
      if Map_ident.is_empty new_map then
        Lam.function_ ~attr ~arity:(List.length params) ~params
          ~body:(convert_aux body)
      else
        let params =
          Ext_list.map params (fun x -> Map_ident.find_default new_map x x)
        in
        Lam.function_ ~attr ~arity:(List.length params) ~params
          ~body:(convert_aux body)
    | Llet (kind, Pgenval, id, e, body) (*FIXME*) -> convert_let kind id e body
    | Lletrec (bindings, body) ->
      let bindings = Ext_list.map_snd bindings convert_aux in
      let body = convert_aux body in
      let lam = Lam.letrec bindings body in
      Lam_scc.scc bindings lam body
    (* inlining will affect how mututal recursive behave *)
    | Lprim (Prevapply, [x; f], outer_loc) | Lprim (Pdirapply, [f; x], outer_loc)
      ->
      convert_pipe f x outer_loc
    | Lprim (Prevapply, _, _) -> assert false
    | Lprim (Pdirapply, _, _) -> assert false
    | Lprim (Pccall a, args, loc) -> convert_ccall a args loc ~dynamic_import
    | Lprim (Pjs_raw_expr, args, loc) -> (
      match args with
      | [Lconst (Const_base (Const_string (code, _)))] ->
        (* js parsing here *)
        let kind = Classify_function.classify code in
        prim ~primitive:(Praw_js_code {code; code_info = Exp kind}) ~args:[] loc
      | _ -> assert false)
    | Lprim (Pjs_raw_stmt, args, loc) -> (
      match args with
      | [Lconst (Const_base (Const_string (code, _)))] ->
        let kind = Classify_function.classify_stmt code in
        prim
          ~primitive:(Praw_js_code {code; code_info = Stmt kind})
          ~args:[] loc
      | _ -> assert false)
    | Lprim (Pgetglobal id, args, _) ->
      let args = Ext_list.map args convert_aux in
      if Ident.is_predef_exn id then
        Lam.const (Const_string {s = id.name; unicode = false})
      else (
        may_depend may_depends (Lam_module_ident.of_ml ~dynamic_import id);
        assert (args = []);
        Lam.global_module ~dynamic_import id)
    | Lprim (Pimport, args, loc) ->
      let args = Ext_list.map args (convert_aux ~dynamic_import:true) in
      lam_prim ~primitive:Pimport ~args loc
    | Lprim (primitive, args, loc) ->
      let args = Ext_list.map args (convert_aux ~dynamic_import) in
      lam_prim ~primitive ~args loc
    | Lswitch (e, s, _loc) -> convert_switch e s
    | Lstringswitch (e, cases, default, _) ->
      Lam.stringswitch (convert_aux e)
        (Ext_list.map_snd cases convert_aux)
        (Ext_option.map default convert_aux)
    | Lstaticraise (id, []) ->
      Lam.staticraise (Hash_int.find_default exit_map id id) []
    | Lstaticraise (id, args) ->
      Lam.staticraise id (Ext_list.map args convert_aux)
    | Lstaticcatch (b, (i, []), Lstaticraise (j, [])) ->
      (* peep-hole [i] aliased to [j] *)
      Hash_int.add exit_map i (Hash_int.find_default exit_map j j);
      convert_aux b
    | Lstaticcatch (b, (i, ids), handler) ->
      Lam.staticcatch (convert_aux b) (i, ids) (convert_aux handler)
    | Ltrywith (b, id, handler) ->
      let body = convert_aux b in
      let handler = convert_aux handler in
      if exception_id_destructed handler id then
        let new_id = Ident.create ("raw_" ^ id.name) in
        Lam.try_ body new_id
          (Lam.let_ StrictOpt id
             (prim ~primitive:Pwrap_exn ~args:[Lam.var new_id] Location.none)
             handler)
      else Lam.try_ body id handler
    | Lifthenelse (b, then_, else_) ->
      Lam.if_ (convert_aux b) (convert_aux then_) (convert_aux else_)
    | Lsequence (a, b) -> Lam.seq (convert_aux a) (convert_aux b)
    | Lwhile (b, body) -> Lam.while_ (convert_aux b) (convert_aux body)
    | Lfor (id, from_, to_, dir, loop) ->
      Lam.for_ id (convert_aux from_) (convert_aux to_) dir (convert_aux loop)
    | Lassign (id, body) -> Lam.assign id (convert_aux body)
  and convert_let (kind : Lam_compat.let_kind) id (e : Lambda.lambda) body :
      Lam.t =
    match (kind, e) with
    | Alias, Lvar u ->
      let new_u = Hash_ident.find_default alias_tbl u u in
      Hash_ident.add alias_tbl id new_u;
      if Set_ident.mem exports id then
        Lam.let_ kind id (Lam.var new_u) (convert_aux body)
      else convert_aux body
    | _, _ -> (
      let new_e = convert_aux e in
      let new_body = convert_aux body in
      (*
            reverse engineering cases as {[           
           (let (switcher/1013 =a (-1+ match/1012))
               (if (isout 2 switcher/1013) (exit 1)
                   (switch* switcher/1013
                      case int 0: 'a'
                        case int 1: 'b'
                        case int 2: 'c')))            
         ]}
         To elemininate the id [switcher], we need ensure it appears only 
         in two places.

         To advance this case, when [sw_failaction] is None
      *)
      match (kind, new_e, new_body) with
      | ( Alias,
          Lprim {primitive = Poffsetint offset; args = [(Lvar _ as matcher)]},
          Lswitch
            ( Lvar switcher3,
              ({
                 sw_consts_full = false;
                 sw_consts;
                 sw_blocks = [];
                 sw_blocks_full = true;
                 sw_failaction = Some ifso;
               } as px) ) )
        when Ident.same switcher3 id
             && (not (Lam_hit.hit_variable id ifso))
             && not (Ext_list.exists_snd sw_consts (Lam_hit.hit_variable id)) ->
        Lam.switch matcher
          {
            px with
            sw_consts =
              Ext_list.map sw_consts (fun (i, act) -> (i - offset, act));
          }
      | _ -> Lam.let_ kind id new_e new_body)
  and convert_pipe (f : Lambda.lambda) (x : Lambda.lambda) outer_loc =
    let x = convert_aux x in
    let f = convert_aux f in
    match f with
    | Lfunction
        {params = [param]; body = Lprim {primitive; args = [Lvar inner_arg]}}
      when Ident.same param inner_arg ->
      Lam.prim ~primitive ~args:[x] outer_loc
    | Lapply
        {
          ap_func =
            Lfunction {params; body = Lprim {primitive; args = inner_args}};
          ap_args = args;
        }
      when Ext_list.for_all2_no_exn inner_args params lam_is_var
           && Ext_list.length_larger_than_n inner_args args 1 ->
      Lam.prim ~primitive ~args:(Ext_list.append_one args x) outer_loc
    | Lapply {ap_func; ap_args; ap_info} ->
      Lam.apply ap_func
        (Ext_list.append_one ap_args x)
        {
          ap_loc = outer_loc;
          ap_inlined = ap_info.ap_inlined;
          ap_status = App_na;
        }
    | _ ->
      Lam.apply f [x]
        {ap_loc = outer_loc; ap_inlined = Default_inline; ap_status = App_na}
  and convert_switch (e : Lambda.lambda) (s : Lambda.lambda_switch) =
    let e = convert_aux e in
    match s with
    | {
     sw_failaction = None;
     sw_blocks = [];
     sw_numblocks = 0;
     sw_consts;
     sw_numconsts;
     sw_names;
    } -> (
      match happens_to_be_diff sw_consts sw_names with
      | Some 0 -> e
      | Some i ->
        prim ~primitive:Paddint
          ~args:[e; Lam.const (Const_int {i = Int32.of_int i; comment = None})]
          Location.none
      | _ ->
        Lam.switch e
          {
            sw_failaction = None;
            sw_blocks = [];
            sw_blocks_full = true;
            sw_consts = Ext_list.map_snd sw_consts convert_aux;
            sw_consts_full = Ext_list.length_ge sw_consts sw_numconsts;
            sw_names = s.sw_names;
          })
    | _ ->
      Lam.switch e
        {
          sw_consts_full = Ext_list.length_ge s.sw_consts s.sw_numconsts;
          sw_consts = Ext_list.map_snd s.sw_consts convert_aux;
          sw_blocks_full = Ext_list.length_ge s.sw_blocks s.sw_numblocks;
          sw_blocks = Ext_list.map_snd s.sw_blocks convert_aux;
          sw_failaction = Ext_option.map s.sw_failaction convert_aux;
          sw_names = s.sw_names;
        }
  in
  (convert_aux lam, may_depends)

(** FIXME: more precise analysis of [id], if it is not 
    used, we can remove it
        only two places emit [Lifused],
    {[
      lsequence (Lifused(id, set_inst_var obj id expr)) rem
        Lifused (env2, Lprim(Parrayset Paddrarray, [Lvar self; Lvar env2; Lvar env1']))
    ]}

        Note the variable, [id], or [env2] is already defined, it can be removed if it is not
        used. This optimization seems useful, but doesnt really matter since it only hit translclass

        more details, see [translclass] and [if_used_test]
        seems to be an optimization trick for [translclass]

        | Lifused(v, l) ->
          if count_var v > 0 then simplif l else lambda_unit
*)

(*
        | Lfunction(kind,params,Lprim(prim,inner_args,inner_loc))
          when List.for_all2_no_exn (fun x y ->
          match y with
          | Lambda.Lvar y when Ident.same x y -> true
          | _ -> false
           ) params inner_args
          ->
          let rec aux outer_args params =
            match outer_args, params with
            | x::xs , _::ys ->
              x :: aux xs ys
            | [], [] -> []
            | x::xs, [] ->
            | [], y::ys
          if Ext_list.same_length inner_args args then
            aux (Lprim(prim,args,inner_loc))
          else

   {[
     (fun x y -> f x y) (computation;e) -->
     (fun y -> f (computation;e) y)
   ]}
   is wrong

   or
   {[
     (fun x y -> f x y ) ([|1;2;3|]) -->
     (fun y -> f [|1;2;3|] y)
   ]}
   is also wrong.

   It seems, we need handle [@variadic] earlier

   or
   {[
     (fun x y -> f x y) ([|1;2;3|]) -->
     let x0, x1, x2 =1,2,3 in
     (fun y -> f [|x0;x1;x2|] y)
   ]}
   But this still need us to know [@variadic] in advance


   we should not remove it immediately, since we have to be careful
   where it is used, it can be [exported], [Lvar] or [Lassign] etc
   The other common mistake is that
   {[
     let x = y (* elimiated x/y*)
     let u = x  (* eliminated u/x *)
   ]}

   however, [x] is already eliminated
   To improve the algorithm
   {[
     let x = y (* x/y *)
     let u = x (* u/y *)
   ]}
   This looks more correct, but lets be conservative here

   global module inclusion {[ include List ]}
   will cause code like {[ let include =a Lglobal_module (list)]}

   when [u] is global, it can not be bound again,
   it should always be the leaf
*)
