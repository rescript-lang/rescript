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
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error = Unknown_builtin_primitive of string

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref
    (fun _cc _rootpath _modl -> assert false
      : module_coercion -> Path.t option -> module_expr -> lambda)

(* Compile an exception/extension definition *)

let transl_extension_constructor env path ext =
  let name =
    match path (*!Clflags.for_package*) with
    | None -> Ident.name ext.ext_id
    | Some p -> Path.name p
  in
  let loc = ext.ext_loc in
  match ext.ext_kind with
  | Text_decl _ -> Lprim (Pcreate_extension name, [], loc)
  | Text_rebind (path, _lid) -> transl_extension_path ~loc env path

(* Translation of primitives *)

(** This is ad-hoc translation for unifying specific primitive operations
     See [Unified_ops] module for detailed explanation.
  *)
let translate_unified_ops (prim : Primitive.description) (env : Env.t)
    (lhs_type : type_expr) : Lambda.primitive option =
  (* lhs_type is already unified in type-level *)
  let entry = Hashtbl.find_opt Unified_ops.index_by_name prim.prim_name in
  match entry with
  | Some {specialization} -> (
    match specialization with
    | {int}
      when is_base_type env lhs_type Predef.path_int
           || maybe_pointer_type env lhs_type = Immediate ->
      Some int
    | {float = Some float} when is_base_type env lhs_type Predef.path_float ->
      Some float
    | {bigint = Some bigint} when is_base_type env lhs_type Predef.path_bigint
      ->
      Some bigint
    | {string = Some string} when is_base_type env lhs_type Predef.path_string
      ->
      Some string
    | {bool = Some bool} when is_base_type env lhs_type Predef.path_bool ->
      Some bool
    | {int} -> Some int)
  | _ -> None

type specialized = {
  objcomp: Lambda.primitive;
  intcomp: Lambda.primitive;
  boolcomp: Lambda.primitive;
  floatcomp: Lambda.primitive;
  stringcomp: Lambda.primitive;
  bigintcomp: Lambda.primitive;
  simplify_constant_constructor: bool;
}

let comparisons_table =
  create_hashtable
    [|
      ( "%equal",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = true;
        } );
      ( "%notequal",
        {
          objcomp = Pobjcomp Cneq;
          intcomp = Pintcomp Cneq;
          boolcomp = Pboolcomp Cneq;
          floatcomp = Pfloatcomp Cneq;
          stringcomp = Pstringcomp Cneq;
          bigintcomp = Pbigintcomp Cneq;
          simplify_constant_constructor = true;
        } );
      ( "%lessthan",
        {
          objcomp = Pobjcomp Clt;
          intcomp = Pintcomp Clt;
          boolcomp = Pboolcomp Clt;
          floatcomp = Pfloatcomp Clt;
          stringcomp = Pstringcomp Clt;
          bigintcomp = Pbigintcomp Clt;
          simplify_constant_constructor = false;
        } );
      ( "%greaterthan",
        {
          objcomp = Pobjcomp Cgt;
          intcomp = Pintcomp Cgt;
          boolcomp = Pboolcomp Cgt;
          floatcomp = Pfloatcomp Cgt;
          stringcomp = Pstringcomp Cgt;
          bigintcomp = Pbigintcomp Cgt;
          simplify_constant_constructor = false;
        } );
      ( "%lessequal",
        {
          objcomp = Pobjcomp Cle;
          intcomp = Pintcomp Cle;
          boolcomp = Pboolcomp Cle;
          floatcomp = Pfloatcomp Cle;
          stringcomp = Pstringcomp Cle;
          bigintcomp = Pbigintcomp Cle;
          simplify_constant_constructor = false;
        } );
      ( "%greaterequal",
        {
          objcomp = Pobjcomp Cge;
          intcomp = Pintcomp Cge;
          boolcomp = Pboolcomp Cge;
          floatcomp = Pfloatcomp Cge;
          stringcomp = Pstringcomp Cge;
          bigintcomp = Pbigintcomp Cge;
          simplify_constant_constructor = false;
        } );
      ( "%compare",
        {
          objcomp = Pobjorder;
          intcomp = Pintorder;
          boolcomp = Pboolorder;
          floatcomp = Pfloatorder;
          stringcomp = Pstringorder;
          bigintcomp = Pbigintorder;
          simplify_constant_constructor = false;
        } );
      ( "%max",
        {
          objcomp = Pobjmax;
          intcomp = Pintmax;
          boolcomp = Pboolmax;
          floatcomp = Pfloatmax;
          stringcomp = Pstringmax;
          bigintcomp = Pbigintmax;
          simplify_constant_constructor = false;
        } );
      ( "%min",
        {
          objcomp = Pobjmin;
          intcomp = Pintmin;
          boolcomp = Pboolmin;
          floatcomp = Pfloatmin;
          stringcomp = Pstringmin;
          bigintcomp = Pbigintmin;
          simplify_constant_constructor = false;
        } );
      ( "%equal_null",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      ( "%equal_undefined",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      ( "%equal_nullable",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      (* FIXME: Core compatibility *)
      ( "%bs_min",
        {
          objcomp = Pobjmin;
          intcomp = Pintmin;
          boolcomp = Pboolmin;
          floatcomp = Pfloatmin;
          stringcomp = Pstringmin;
          bigintcomp = Pbigintmin;
          simplify_constant_constructor = false;
        } );
      ( "%bs_max",
        {
          objcomp = Pobjmax;
          intcomp = Pintmax;
          boolcomp = Pboolmax;
          floatcomp = Pfloatmax;
          stringcomp = Pstringmax;
          bigintcomp = Pbigintmax;
          simplify_constant_constructor = false;
        } );
    |]

let primitives_table =
  create_hashtable
    [|
      ("%identity", Pidentity);
      ("%ignore", Pignore);
      ("%revapply", Prevapply);
      ("%apply", Pdirapply);
      ("%loc_LOC", Ploc Loc_LOC);
      ("%loc_FILE", Ploc Loc_FILE);
      ("%loc_LINE", Ploc Loc_LINE);
      ("%loc_POS", Ploc Loc_POS);
      ("%loc_MODULE", Ploc Loc_MODULE);
      (* BEGIN Triples for  ref data type *)
      ("%makeref", Pmakeblock Lambda.ref_tag_info);
      ("%refset", Psetfield (0, Lambda.ref_field_set_info));
      ("%refget", Pfield (0, Lambda.ref_field_info));
      ("%incr", Poffsetref 1);
      ("%decr", Poffsetref (-1));
      (* Finish Triples for  ref data type *)
      ("%field0", Pfield (0, Fld_tuple));
      ("%field1", Pfield (1, Fld_tuple));
      ("%obj_dup", Pduprecord);
      ("%obj_tag", Pobjtag);
      ("%obj_size", Pobjsize);
      ("%obj_get_field", Parrayrefu);
      ("%obj_set_field", Parraysetu);
      ("%raise", Praise Raise_regular);
      (* bool primitives *)
      ("%sequand", Psequand);
      ("%sequor", Psequor);
      ("%boolnot", Pnot);
      ("%boolorder", Pboolorder);
      ("%boolmin", Pboolmin);
      ("%boolmax", Pboolmax);
      (* int primitives *)
      ("%obj_is_int", Pisint);
      ("%negint", Pnegint);
      ("%succint", Poffsetint 1);
      ("%predint", Poffsetint (-1));
      ("%addint", Paddint);
      ("%subint", Psubint);
      ("%mulint", Pmulint);
      ("%divint", Pdivint);
      ("%modint", Pmodint);
      ("%andint", Pandint);
      ("%orint", Porint);
      ("%xorint", Pxorint);
      ("%lslint", Plslint);
      ("%lsrint", Plsrint);
      ("%asrint", Pasrint);
      ("%eq", Pintcomp Ceq);
      ("%noteq", Pintcomp Cneq);
      ("%ltint", Pintcomp Clt);
      ("%leint", Pintcomp Cle);
      ("%gtint", Pintcomp Cgt);
      ("%geint", Pintcomp Cge);
      ("%intorder", Pintorder);
      ("%intmin", Pintmin);
      ("%intmax", Pintmax);
      (* float primitives *)
      ("%negfloat", Pnegfloat);
      ("%absfloat", Pabsfloat);
      ("%addfloat", Paddfloat);
      ("%subfloat", Psubfloat);
      ("%mulfloat", Pmulfloat);
      ("%divfloat", Pdivfloat);
      ("%modfloat", Pmodfloat);
      ("%eqfloat", Pfloatcomp Ceq);
      ("%noteqfloat", Pfloatcomp Cneq);
      ("%ltfloat", Pfloatcomp Clt);
      ("%lefloat", Pfloatcomp Cle);
      ("%gtfloat", Pfloatcomp Cgt);
      ("%gefloat", Pfloatcomp Cge);
      ("%floatorder", Pfloatorder);
      ("%floatmin", Pfloatmin);
      ("%floatmax", Pfloatmax);
      (* bigint primitives *)
      ("%negbigint", Pnegbigint);
      ("%addbigint", Paddbigint);
      ("%subbigint", Psubbigint);
      ("%mulbigint", Pmulbigint);
      ("%divbigint", Pdivbigint);
      ("%powbigint", Ppowbigint);
      ("%modbigint", Pmodbigint);
      ("%eqbigint", Pbigintcomp Ceq);
      ("%noteqbigint", Pbigintcomp Cneq);
      ("%ltbigint", Pbigintcomp Clt);
      ("%lebigint", Pbigintcomp Cle);
      ("%gtbigint", Pbigintcomp Cgt);
      ("%gebigint", Pbigintcomp Cge);
      ("%andbigint", Pandbigint);
      ("%orbigint", Porbigint);
      ("%xorbigint", Pxorbigint);
      ("%lslbigint", Plslbigint);
      ("%asrbigint", Pasrbigint);
      ("%bigintorder", Pbigintorder);
      ("%bigintmin", Pbigintmin);
      ("%bigintmax", Pbigintmax);
      (* string primitives *)
      ("%string_length", Pstringlength);
      ("%string_safe_get", Pstringrefs);
      ("%string_unsafe_get", Pstringrefu);
      ("%stringorder", Pstringorder);
      ("%stringmin", Pstringmin);
      ("%stringmax", Pstringmax);
      ("%string_concat", Pstringadd);
      (* array primitives *)
      ("%array_length", Parraylength);
      ("%array_safe_get", Parrayrefs);
      ("%array_safe_set", Parraysets);
      ("%array_unsafe_get", Parrayrefu);
      ("%array_unsafe_set", Parraysetu);
      (* dict primitives *)
      ("%makedict", Pmakedict);
      (* promise *)
      ("%await", Pawait);
      (* module *)
      ("%import", Pimport);
      (* hash *)
      ("%hash", Phash);
      ("%hash_mix_int", Phash_mixint);
      ("%hash_mix_string", Phash_mixstring);
      ("%hash_final_mix", Phash_finalmix);
      (* etc *)
      ("%typeof", Ptypeof);
      ("%debugger", Pdebugger);
      ("%intoffloat", Pintoffloat);
      ("%floatofint", Pfloatofint);
      ("%unsafe_eq", Pjscomp Ceq);
      ("%unsafe_neq", Pjscomp Cneq);
      ("%unsafe_lt", Pjscomp Clt);
      ("%unsafe_le", Pjscomp Cle);
      ("%unsafe_gt", Pjscomp Cgt);
      ("%unsafe_ge", Pjscomp Cge);
      ("%null", Pnull);
      ("%undefined", Pundefined);
      ("%is_nullable", Pisnullable);
      ("%undefined_to_opt", Pundefined_to_opt);
      ("%null_to_opt", Pnull_to_opt);
      ("%nullable_to_opt", Pnullable_to_opt);
      ("%function_arity", Pfn_arity);
      ("%wrap_exn", Pwrap_exn);
      ("%curry_apply1", Pcurry_apply 1);
      ("%curry_apply2", Pcurry_apply 2);
      ("%curry_apply3", Pcurry_apply 3);
      ("%curry_apply4", Pcurry_apply 4);
      ("%curry_apply5", Pcurry_apply 5);
      ("%curry_apply6", Pcurry_apply 6);
      ("%curry_apply7", Pcurry_apply 7);
      ("%curry_apply8", Pcurry_apply 8);
      ("%makemutablelist", Pmakelist Mutable);
      ("%unsafe_to_method", Pjs_fn_method);
      (* Compiler internals, never expose to ReScript files *)
      ("#raw_expr", Pjs_raw_expr);
      ("#raw_stmt", Pjs_raw_stmt);
      (* FIXME: Core compatibility *)
      ("#null", Pnull);
      ("#undefined", Pundefined);
      ("#typeof", Ptypeof);
      ("#is_nullable", Pisnullable);
      ("#null_to_opt", Pnull_to_opt);
      ("#nullable_to_opt", Pnullable_to_opt);
      ("#undefined_to_opt", Pundefined_to_opt);
      ("#makemutablelist", Pmakelist Mutable);
      ("#import", Pimport);
      (* FIXME: Deprecated *)
      ("%obj_field", Parrayrefu);
    |]

let find_primitive prim_name = Hashtbl.find primitives_table prim_name

let specialize_comparison
    ({objcomp; intcomp; floatcomp; stringcomp; bigintcomp; boolcomp} :
      specialized) env ty =
  match () with
  | ()
    when is_base_type env ty Predef.path_int
         || is_base_type env ty Predef.path_char
         || maybe_pointer_type env ty = Immediate ->
    intcomp
  | () when is_base_type env ty Predef.path_float -> floatcomp
  | () when is_base_type env ty Predef.path_string -> stringcomp
  | () when is_base_type env ty Predef.path_bigint -> bigintcomp
  | () when is_base_type env ty Predef.path_bool -> boolcomp
  | () -> objcomp

(* Specialize a primitive from available type information,
   raise Not_found if primitive is unknown *)

let specialize_primitive p env ty (* ~has_constant_constructor *) =
  let fn_expr = is_function_type env ty in
  let unified =
    match fn_expr with
    | Some (lhs, _) -> translate_unified_ops p env lhs
    | None -> None
  in
  match unified with
  | Some primitive -> primitive
  | None -> (
    try
      let table = Hashtbl.find comparisons_table p.prim_name in
      match fn_expr with
      | Some (lhs, _rhs) -> specialize_comparison table env lhs
      | None -> table.objcomp
    with Not_found -> find_primitive p.prim_name)

(* Eta-expand a primitive *)

let transl_primitive loc p env ty =
  (* Printf.eprintf "----transl_primitive %s----\n" p.prim_name; *)
  let prim =
    try specialize_primitive p env ty (* ~has_constant_constructor:false *)
    with Not_found -> Pccall p
  in
  match prim with
  | Ploc kind -> (
    let lam = lam_of_loc kind loc in
    match p.prim_arity with
    | 0 -> lam
    | 1 ->
      (* TODO: we should issue a warning ? *)
      let param = Ident.create "prim" in
      Lfunction
        {
          params = [param];
          attr = default_function_attribute;
          loc;
          body = Lprim (Pmakeblock Blk_tuple, [lam; Lvar param], loc);
        }
    | _ -> assert false)
  | _ ->
    let rec make_params n total =
      if n <= 0 then []
      else
        Ident.create ("prim" ^ string_of_int (total - n))
        :: make_params (n - 1) total
    in
    let prim_arity = p.prim_arity in
    if p.prim_from_constructor || prim_arity = 0 then Lprim (prim, [], loc)
    else
      let params =
        if prim_arity = 1 then [Ident.create "prim"]
        else make_params prim_arity prim_arity
      in
      Lfunction
        {
          params;
          attr = default_function_attribute;
          loc;
          body = Lprim (prim, List.map (fun id -> Lvar id) params, loc);
        }

let transl_primitive_application loc prim env ty args =
  let prim_name = prim.prim_name in
  let unified =
    match args with
    | [arg1] | [arg1; _] -> translate_unified_ops prim env arg1.exp_type
    | _ -> None
  in
  match unified with
  | Some primitive -> primitive
  | None -> (
    try
      match args with
      | [arg1; _]
        when is_base_type env arg1.exp_type Predef.path_bool
             && Hashtbl.mem comparisons_table prim_name ->
        (Hashtbl.find comparisons_table prim_name).boolcomp
      | _ ->
        let has_constant_constructor =
          match args with
          | [
              _; {exp_desc = Texp_construct (_, {cstr_tag = Cstr_constant _}, _)};
            ]
          | [
              {exp_desc = Texp_construct (_, {cstr_tag = Cstr_constant _}, _)}; _;
            ]
          | [_; {exp_desc = Texp_variant (_, None)}]
          | [{exp_desc = Texp_variant (_, None)}; _] ->
            true
          | _ -> false
        in
        if has_constant_constructor then
          match Hashtbl.find_opt comparisons_table prim_name with
          | Some table when table.simplify_constant_constructor -> table.intcomp
          | Some _ | None -> specialize_primitive prim env ty
          (* ~has_constant_constructor*)
        else specialize_primitive prim env ty
    with Not_found ->
      if String.length prim_name > 0 && prim_name.[0] = '%' then
        raise (Error (loc, Unknown_builtin_primitive prim_name));
      Pccall prim)

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
  | Lconst sc -> sc
  | _ -> raise_notrace Not_constant

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

type binding =
  | Bind_value of value_binding list
  | Bind_module of Ident.t * string loc * module_expr

let rec push_defaults loc bindings case partial =
  match case with
  | {
   c_lhs = pat;
   c_guard = None;
   c_rhs =
     {exp_desc = Texp_function {arg_label; arity; param; case; partial; async}}
     as exp;
  } ->
    let case = push_defaults exp.exp_loc bindings case partial in

    {
      c_lhs = pat;
      c_guard = None;
      c_rhs =
        {
          exp with
          exp_desc =
            Texp_function {arg_label; arity; param; case; partial; async};
        };
    }
  | {
   c_lhs = pat;
   c_guard = None;
   c_rhs =
     {
       exp_attributes = [({txt = "#default"}, _)];
       exp_desc =
         Texp_let (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2));
     };
  } ->
    push_defaults loc
      (Bind_value binds :: bindings)
      {c_lhs = pat; c_guard = None; c_rhs = e2}
      partial
  | {
   c_lhs = pat;
   c_guard = None;
   c_rhs =
     {
       exp_attributes = [({txt = "#modulepat"}, _)];
       exp_desc =
         Texp_letmodule (id, name, mexpr, ({exp_desc = Texp_function _} as e2));
     };
  } ->
    push_defaults loc
      (Bind_module (id, name, mexpr) :: bindings)
      {c_lhs = pat; c_guard = None; c_rhs = e2}
      partial
  | case ->
    let exp =
      List.fold_left
        (fun exp binds ->
          {
            exp with
            exp_desc =
              (match binds with
              | Bind_value binds -> Texp_let (Nonrecursive, binds, exp)
              | Bind_module (id, name, mexpr) ->
                Texp_letmodule (id, name, mexpr, exp));
          })
        case.c_rhs bindings
    in
    {case with c_rhs = exp}

(* Assertions *)

let assert_failed exp =
  let fname, line, char =
    Location.get_pos_info exp.exp_loc.Location.loc_start
  in
  let fname = Filename.basename fname in
  Lprim
    ( Praise Raise_regular,
      [
        Lprim
          ( Pmakeblock Blk_extension,
            [
              transl_normal_path Predef.path_assert_failure;
              Lconst
                (Const_block
                   ( Blk_tuple,
                     [
                       Const_base (Const_string (fname, None));
                       Const_base (Const_int line);
                       Const_base (Const_int char);
                     ] ));
            ],
            exp.exp_loc );
      ],
      exp.exp_loc )

let rec cut n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> failwith "Translcore.cut"
    | a :: l ->
      let l1, l2 = cut (n - 1) l in
      (a :: l1, l2)

(* Translation of expressions *)

let try_ids = Hashtbl.create 8

let extract_directive_for_fn exp =
  exp.exp_attributes
  |> List.find_map (fun ({txt}, payload) ->
         if txt = "directive" then Ast_payload.is_single_string payload
         else None)

let rec transl_exp e =
  List.iter (Translattribute.check_attribute e) e.exp_attributes;
  transl_exp0 e

and transl_exp0 (e : Typedtree.expression) : Lambda.lambda =
  match e.exp_desc with
  | Texp_ident (_, _, {val_kind = Val_prim p}) ->
    transl_primitive e.exp_loc p e.exp_env e.exp_type
  | Texp_ident (path, _, {val_kind = Val_reg}) ->
    transl_value_path ~loc:e.exp_loc e.exp_env path
  | Texp_constant cst -> Lconst (Const_base cst)
  | Texp_let (rec_flag, pat_expr_list, body) ->
    transl_let rec_flag pat_expr_list (transl_exp body)
  | Texp_function {arg_label = _; arity; param; case; partial; async} -> (
    let directive =
      match extract_directive_for_fn e with
      | None -> None
      | Some (directive, _) -> Some directive
    in
    let params, body, return_unit =
      let pl = push_defaults e.exp_loc [] case partial in
      transl_function e.exp_loc partial param pl
    in
    let attr =
      {
        default_function_attribute with
        inline = Translattribute.get_inline_attribute e.exp_attributes;
        async;
        return_unit;
        directive;
      }
    in
    let loc = e.exp_loc in
    let lambda = Lfunction {params; body; attr; loc} in
    match arity with
    | Some arity ->
      let prim =
        let expanded = Ctype.expand_head e.exp_env e.exp_type in
        match (Btype.repr expanded).desc with
        | Tarrow (Nolabel, t, _, _, _) -> (
          match (Ctype.expand_head e.exp_env t).desc with
          | Tconstr (Pident {name = "unit"}, [], _) -> Pjs_fn_make_unit
          | _ -> Pjs_fn_make arity)
        | _ -> Pjs_fn_make arity
      in
      Lprim
        ( prim (* could be replaced with Opaque in the future except arity 0*),
          [lambda],
          loc )
    | None -> lambda)
  | Texp_apply
      {
        funct =
          {
            exp_desc = Texp_ident (_, _, {val_kind = Val_prim p});
            exp_type = prim_type;
          } as funct;
        args = oargs;
      }
    when List.length oargs >= p.prim_arity
         && List.for_all (fun (_, arg) -> arg <> None) oargs -> (
    let args, args' = cut p.prim_arity oargs in
    let wrap f =
      if args' = [] then f
      else
        let inlined, _ =
          Translattribute.get_and_remove_inlined_attribute funct
        in
        transl_apply ~inlined f args' e.exp_loc
    in
    let args =
      List.map
        (function
          | _, Some x -> x
          | _ -> assert false)
        args
    in
    let argl = transl_list args in
    let prim =
      transl_primitive_application e.exp_loc p e.exp_env prim_type args
    in
    match (prim, args) with
    | Praise k, [_] ->
      let targ = List.hd argl in
      let k =
        match (k, targ) with
        | Raise_regular, Lvar id when Hashtbl.mem try_ids id -> Raise_reraise
        | _ -> k
      in
      wrap (Lprim (Praise k, [targ], e.exp_loc))
    | Ploc kind, [] -> lam_of_loc kind e.exp_loc
    | Ploc kind, [arg1] ->
      let lam = lam_of_loc kind arg1.exp_loc in
      Lprim (Pmakeblock Blk_tuple, lam :: argl, e.exp_loc)
    | Ploc _, _ -> assert false
    | _, _ -> (
      match (prim, argl) with
      | _ -> wrap (Lprim (prim, argl, e.exp_loc))))
  | Texp_apply {funct; args = oargs; partial} ->
    let inlined, funct =
      Translattribute.get_and_remove_inlined_attribute funct
    in
    let uncurried_partial_application =
      (* In case of partial application foo(args, ...) when some args are missing,
         get the arity *)
      if partial then
        let arity_opt = Ctype.get_arity funct.exp_env funct.exp_type in
        match arity_opt with
        | Some arity ->
          let real_args = List.filter (fun (_, x) -> Option.is_some x) oargs in
          if arity > List.length real_args then Some arity else None
        | None -> None
      else None
    in
    transl_apply ~inlined ~uncurried_partial_application (transl_exp funct)
      oargs e.exp_loc
  | Texp_match (arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try (body, pat_expr_list) ->
    let id = Typecore.name_pattern "exn" pat_expr_list in
    Ltrywith
      ( transl_exp body,
        id,
        Matching.for_trywith (Lvar id) (transl_cases_try pat_expr_list) )
  | Texp_tuple el -> (
    let ll = transl_list el in
    try Lconst (Const_block (Blk_tuple, List.map extract_constant ll))
    with Not_constant -> Lprim (Pmakeblock Blk_tuple, ll, e.exp_loc))
  | Texp_construct ({txt = Lident "false"}, _, []) -> Lconst Const_false
  | Texp_construct ({txt = Lident "true"}, _, []) -> Lconst Const_true
  | Texp_construct (lid, cstr, args) -> (
    let ll = transl_list args in
    if cstr.cstr_inlined <> None then
      match ll with
      | [x] -> x
      | _ -> assert false
    else
      match cstr.cstr_tag with
      | Cstr_constant n ->
        Lconst
          (Const_pointer
             ( n,
               match lid.txt with
               | Longident.Ldot (Longident.Lident "*predef*", "None")
               | Longident.Lident "None"
                 when Datarepr.constructor_has_optional_shape cstr ->
                 Pt_shape_none
               | _ ->
                 if Datarepr.constructor_has_optional_shape cstr then
                   Pt_shape_none
                 else
                   Pt_constructor
                     {
                       name = cstr.cstr_name;
                       const = cstr.cstr_consts;
                       non_const = cstr.cstr_nonconsts;
                       attrs = cstr.cstr_attributes;
                     } ))
      | Cstr_unboxed -> (
        match ll with
        | [v] -> v
        | _ -> assert false)
      | Cstr_block n -> (
        let tag_info : Lambda.tag_info =
          if Datarepr.constructor_has_optional_shape cstr then
            match args with
            | [arg]
              when Typeopt.type_cannot_contain_undefined arg.exp_type
                     arg.exp_env ->
              (* Format.fprintf Format.err_formatter "@[special boxingl@]@."; *)
              Blk_some_not_nested
            | _ -> Blk_some
          else
            Blk_constructor
              {
                name = cstr.cstr_name;
                num_nonconst = cstr.cstr_nonconsts;
                tag = n;
                attrs = cstr.cstr_attributes;
              }
        in
        try Lconst (Const_block (tag_info, List.map extract_constant ll))
        with Not_constant -> Lprim (Pmakeblock tag_info, ll, e.exp_loc))
      | Cstr_extension path ->
        Lprim
          ( Pmakeblock Blk_extension,
            transl_extension_path e.exp_env path :: ll,
            e.exp_loc ))
  | Texp_extension_constructor (_, path) -> transl_extension_path e.exp_env path
  | Texp_variant (l, arg) -> (
    let tag = Btype.hash_variant l in
    match arg with
    | None -> Lconst (Const_pointer (tag, Pt_variant {name = l}))
    | Some arg -> (
      let lam = transl_exp arg in
      let tag_info = Blk_poly_var l in
      try
        Lconst
          (Const_block
             (tag_info, [Const_base (Const_int tag); extract_constant lam]))
      with Not_constant ->
        Lprim
          ( Pmakeblock tag_info,
            [Lconst (Const_base (Const_int tag)); lam],
            e.exp_loc )))
  | Texp_record {fields; representation; extended_expression} ->
    transl_record e.exp_loc e.exp_env fields representation extended_expression
  | Texp_field (arg, _, lbl) -> (
    let targ = transl_exp arg in
    match lbl.lbl_repres with
    | Record_float_unused -> assert false
    | Record_regular ->
      Lprim (Pfield (lbl.lbl_pos, Lambda.fld_record lbl), [targ], e.exp_loc)
    | Record_inlined _ ->
      Lprim
        (Pfield (lbl.lbl_pos, Lambda.fld_record_inline lbl), [targ], e.exp_loc)
    | Record_unboxed _ -> targ
    | Record_extension ->
      Lprim
        ( Pfield (lbl.lbl_pos + 1, Lambda.fld_record_extension lbl),
          [targ],
          e.exp_loc ))
  | Texp_setfield (arg, _, lbl, newval) ->
    let access =
      match lbl.lbl_repres with
      | Record_float_unused -> assert false
      | Record_regular -> Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
      | Record_inlined _ ->
        Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
      | Record_unboxed _ -> assert false
      | Record_extension ->
        Psetfield (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
    in
    Lprim (access, [transl_exp arg; transl_exp newval], e.exp_loc)
  | Texp_array expr_list ->
    let ll = transl_list expr_list in
    Lprim (Pmakearray Mutable, ll, e.exp_loc)
  | Texp_ifthenelse (cond, ifso, Some ifnot) ->
    Lifthenelse (transl_exp cond, transl_exp ifso, transl_exp ifnot)
  | Texp_ifthenelse (cond, ifso, None) ->
    Lifthenelse (transl_exp cond, transl_exp ifso, lambda_unit)
  | Texp_sequence (expr1, expr2) ->
    Lsequence (transl_exp expr1, transl_exp expr2)
  | Texp_while (cond, body) -> Lwhile (transl_exp cond, transl_exp body)
  | Texp_for (param, _, low, high, dir, body) ->
    Lfor (param, transl_exp low, transl_exp high, dir, transl_exp body)
  | Texp_send (expr, Tmeth_name nm, _) ->
    let obj = transl_exp expr in
    Lsend (nm, obj, e.exp_loc)
  | Texp_letmodule (id, _loc, modl, body) ->
    let defining_expr = !transl_module Tcoerce_none None modl in
    Llet (Strict, Pgenval, id, defining_expr, transl_exp body)
  | Texp_letexception (cd, body) ->
    Llet
      ( Strict,
        Pgenval,
        cd.ext_id,
        transl_extension_constructor e.exp_env None cd,
        transl_exp body )
  | Texp_pack modl -> !transl_module Tcoerce_none None modl
  | Texp_assert {exp_desc = Texp_construct (_, {cstr_name = "false"}, _)} ->
    if !Clflags.no_assert_false then Lambda.lambda_assert_false
    else assert_failed e
  | Texp_assert cond ->
    if !Clflags.noassert then lambda_unit
    else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
    (* when e needs no computation (constants, identifiers, ...), we
       optimize the translation just as Lazy.lazy_from_val would
       do *)
    Lprim (Pmakeblock Blk_lazy_general, [transl_exp e], e.exp_loc)

and transl_list expr_list = List.map transl_exp expr_list

and transl_guard guard rhs =
  let expr = transl_exp rhs in
  match guard with
  | None -> expr
  | Some cond -> Lifthenelse (transl_exp cond, expr, staticfail)

and transl_case {c_lhs; c_guard; c_rhs} = (c_lhs, transl_guard c_guard c_rhs)

and transl_cases cases = List.map transl_case cases

and transl_case_try {c_lhs; c_guard; c_rhs} =
  match c_lhs.pat_desc with
  | Tpat_var (id, _) | Tpat_alias (_, id, _) ->
    Hashtbl.replace try_ids id ();
    Misc.try_finally
      (fun () -> (c_lhs, transl_guard c_guard c_rhs))
      (fun () -> Hashtbl.remove try_ids id)
  | _ -> (c_lhs, transl_guard c_guard c_rhs)

and transl_cases_try cases = List.map transl_case_try cases

and transl_apply ?(inlined = Default_inline)
    ?(uncurried_partial_application = None) lam sargs loc =
  let lapply ap_func ap_args =
    Lapply {ap_loc = loc; ap_func; ap_args; ap_inlined = inlined}
  in
  let rec build_apply lam args = function
    | (None, optional) :: l ->
      let defs = ref [] in
      let protect name lam =
        match lam with
        | Lvar _ | Lconst _ -> lam
        | _ ->
          let id = Ident.create name in
          defs := (id, lam) :: !defs;
          Lvar id
      in
      let args, args' =
        if List.for_all (fun (_, opt) -> opt) args then ([], args)
        else (args, [])
      in
      let lam = if args = [] then lam else lapply lam (List.rev_map fst args) in
      let handle = protect "func" lam
      and l = List.map (fun (arg, opt) -> (may_map (protect "arg") arg, opt)) l
      and id_arg = Ident.create "param" in
      let body =
        match build_apply handle ((Lvar id_arg, optional) :: args') l with
        | Lfunction {params = ids; body = lam; attr; loc} ->
          Lfunction {params = id_arg :: ids; body = lam; attr; loc}
        | lam ->
          Lfunction
            {
              params = [id_arg];
              body = lam;
              attr = default_function_attribute;
              loc;
            }
      in
      List.fold_left
        (fun body (id, lam) -> Llet (Strict, Pgenval, id, lam, body))
        body !defs
    | (Some arg, optional) :: l -> build_apply lam ((arg, optional) :: args) l
    | [] -> lapply lam (List.rev_map fst args)
  in
  match uncurried_partial_application with
  | Some arity ->
    let extra_arity = arity - List.length sargs in
    let none_ids = ref [] in
    let args =
      Ext_list.filter_map sargs (function
        | _, Some e -> Some (transl_exp e)
        | _, None ->
          let id_arg = Ident.create "none" in
          none_ids := id_arg :: !none_ids;
          Some (Lvar id_arg))
    in
    let extra_ids =
      Array.init extra_arity (fun _ -> Ident.create "extra") |> Array.to_list
    in
    let extra_args = Ext_list.map extra_ids (fun id -> Lvar id) in
    let ap_args = args @ extra_args in
    let l0 =
      Lapply {ap_func = lam; ap_args; ap_inlined = inlined; ap_loc = loc}
    in
    Lfunction
      {
        params = List.rev_append !none_ids extra_ids;
        body = l0;
        attr = default_function_attribute;
        loc;
      }
  | _ ->
    (build_apply lam []
       (List.map
          (fun (l, x) -> (may_map transl_exp x, Btype.is_optional l))
          sargs)
      : Lambda.lambda)

and transl_function loc partial param case =
  match case with
  | {
   c_lhs = pat;
   c_guard = None;
   c_rhs =
     {
       exp_desc =
         Texp_function
           {
             arg_label = _;
             arity = None;
             param = param';
             case;
             partial = partial';
             async;
           };
     } as exp;
  }
    when Parmatch.inactive ~partial pat && not async ->
    let params, body, return_unit =
      transl_function exp.exp_loc partial' param' case
    in
    ( param :: params,
      Matching.for_function loc None (Lvar param) [(pat, body)] partial,
      return_unit )
  | {c_rhs = {exp_env; exp_type}; _} ->
    ( [param],
      Matching.for_function loc None (Lvar param) [transl_case case] partial,
      is_base_type exp_env exp_type Predef.path_unit )

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
  | Nonrecursive ->
    let rec transl = function
      | [] -> body
      | {vb_pat = pat; vb_expr = expr; vb_attributes = attr; vb_loc} :: rem ->
        let lam = transl_exp expr in
        let lam = Translattribute.add_inline_attribute lam vb_loc attr in
        Matching.for_let pat.pat_loc lam pat (transl rem)
    in
    transl pat_expr_list
  | Recursive ->
    let transl_case {vb_expr = expr; vb_attributes; vb_loc; vb_pat = pat} =
      let id =
        match pat.pat_desc with
        | Tpat_var (id, _) -> id
        | Tpat_alias ({pat_desc = Tpat_any}, id, _) -> id
        | _ -> assert false
        (* Illegal_letrec_pat
           Only variables are allowed as left-hand side of `let rec'
        *)
      in
      let lam = transl_exp expr in
      let lam = Translattribute.add_inline_attribute lam vb_loc vb_attributes in
      (id, lam)
    in
    Lletrec (Ext_list.map pat_expr_list transl_case, body)

and transl_record loc env fields repres opt_init_expr =
  match (opt_init_expr, repres, fields) with
  | None, Record_unboxed _, [|({lbl_name; lbl_loc}, Overridden (_, expr), _)|]
    ->
    (* ReScript uncurried encoding *)
    let loc = lbl_loc in
    let lambda = transl_exp expr in
    if lbl_name.[0] = 'I' then
      let arity_s = String.sub lbl_name 1 (String.length lbl_name - 1) in
      let arity = Int32.of_string arity_s |> Int32.to_int in
      Lprim
        ( Pjs_fn_make arity,
          (* could be replaced with Opaque in the future except arity 0*)
          [lambda],
          loc )
    else lambda
  | _ -> (
    let size = Array.length fields in
    let optional =
      Ext_array.exists fields (fun (ld, _, _) -> ld.lbl_optional)
    in
    (* Determine if there are "enough" fields (only relevant if this is a
       functional-style record update *)
    let no_init =
      match opt_init_expr with
      | None -> true
      | _ -> false
    in
    if
      no_init || (size < 20 && not optional)
      (* TODO: More strategies
         3 + 2 * List.length lbl_expr_list >= size (density)
      *)
    then
      (* Allocate new record with given fields (and remaining fields
         taken from init_expr if any *)
      let init_id = Ident.create "init" in
      let lv =
        Array.mapi
          (fun i (lbl, definition, _) ->
            match definition with
            | Kept _ ->
              let access =
                match repres with
                | Record_float_unused -> assert false
                | Record_regular -> Pfield (i, Lambda.fld_record lbl)
                | Record_inlined _ -> Pfield (i, Lambda.fld_record_inline lbl)
                | Record_unboxed _ -> assert false
                | Record_extension ->
                  Pfield (i + 1, Lambda.fld_record_extension lbl)
              in
              Lprim (access, [Lvar init_id], loc)
            | Overridden (_lid, expr) -> transl_exp expr)
          fields
      in
      let ll = Array.to_list lv in
      let mut =
        if Array.exists (fun (lbl, _, _) -> lbl.lbl_mut = Mutable) fields then
          Mutable
        else Immutable
      in
      let lam =
        try
          if mut = Mutable then raise Not_constant;
          let cl = List.map extract_constant ll in
          match repres with
          | Record_float_unused -> assert false
          | Record_regular ->
            Lconst (Const_block (Lambda.blk_record fields mut, cl))
          | Record_inlined {tag; name; num_nonconsts; attrs} ->
            Lconst
              (Const_block
                 ( Lambda.blk_record_inlined fields name num_nonconsts ~tag
                     ~attrs mut,
                   cl ))
          | Record_unboxed _ ->
            Lconst
              (match cl with
              | [v] -> v
              | _ -> assert false)
          | Record_extension -> raise Not_constant
        with Not_constant -> (
          match repres with
          | Record_regular ->
            Lprim (Pmakeblock (Lambda.blk_record fields mut), ll, loc)
          | Record_float_unused -> assert false
          | Record_inlined {tag; name; num_nonconsts; attrs} ->
            Lprim
              ( Pmakeblock
                  (Lambda.blk_record_inlined fields name num_nonconsts ~tag
                     ~attrs mut),
                ll,
                loc )
          | Record_unboxed _ -> (
            match ll with
            | [v] -> v
            | _ -> assert false)
          | Record_extension ->
            let path =
              let label, _, _ = fields.(0) in
              match label.lbl_res.desc with
              | Tconstr (p, _, _) -> p
              | _ -> assert false
            in
            let slot = transl_extension_path env path in
            Lprim
              (Pmakeblock (Lambda.blk_record_ext fields mut), slot :: ll, loc))
      in
      match opt_init_expr with
      | None -> lam
      | Some init_expr ->
        Llet (Strict, Pgenval, init_id, transl_exp init_expr, lam)
    else
      (* Take a shallow copy of the init record, then mutate the fields
         of the copy *)
      let copy_id = Ident.create "newrecord" in
      let update_field cont (lbl, definition, _opt) =
        match definition with
        | Kept _type -> cont
        | Overridden (_lid, expr) ->
          let upd =
            match repres with
            | Record_float_unused -> assert false
            | Record_regular ->
              Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
            | Record_inlined _ ->
              Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
            | Record_unboxed _ -> assert false
            | Record_extension ->
              Psetfield (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
          in
          Lsequence (Lprim (upd, [Lvar copy_id; transl_exp expr], loc), cont)
      in
      match opt_init_expr with
      | None -> assert false
      | Some init_expr ->
        Llet
          ( Strict,
            Pgenval,
            copy_id,
            Lprim (Pduprecord, [transl_exp init_expr], loc),
            Array.fold_left update_field (Lvar copy_id) fields ))

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = Typecore.name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases_try exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    Lstaticcatch
      ( Ltrywith
          ( Lstaticraise (static_exception_id, body),
            id,
            Matching.for_trywith (Lvar id) exn_cases ),
        (static_exception_id, val_ids),
        handler )
  in
  match (arg, exn_cases) with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> Typecore.name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list argl) val_ids
      (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
    Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
    let val_id = Typecore.name_pattern "val" pat_expr_list in
    static_catch
      [transl_exp arg]
      [val_id]
      (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)

open Format

let report_error ppf = function
  | Unknown_builtin_primitive prim_name ->
    fprintf ppf "Unknown builtin primitive \"%s\"" prim_name

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
