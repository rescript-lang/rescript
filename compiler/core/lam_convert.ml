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

let prim = Lam.prim

(* type required_modules = Lam_module_ident.Hash_set.t *)

(** drop Lseq (List! ) etc 
    see #3852, we drop all these required global modules
    but added it back based on our own module analysis
*)
let lam_prim ~primitive:(p : Lambda.primitive) ~args loc : Lam.t =
  match p with
  | Peliminated e -> prim ~primitive:(Peliminated e) ~args loc
  | Pnull -> Lam.const Const_js_null
  | Pundefined -> Lam.const (Const_js_undefined {is_unit = false})
  | Pcreate_extension s -> prim ~primitive:(Pcreate_extension s) ~args loc
  | Pgetglobal _ -> assert false
  | Pmakeblock info -> (
    let mutable_flag = Lambda.mutable_flag_of_tag_info info in
    match info with
    | Blk_some_not_nested -> prim ~primitive:Psome_not_nest ~args loc
    | Blk_some -> prim ~primitive:Psome ~args loc
    | Blk_constructor _ | Blk_tuple | Blk_record _ | Blk_record_inlined _
    | Blk_module _ | Blk_module_export _ | Blk_extension | Blk_record_ext _ ->
      prim ~primitive:(Pmakeblock (info, mutable_flag)) ~args loc
    | Blk_poly_var s -> (
      match args with
      | [_; value] ->
        let tag_val : Lam_constant.t =
          if Ext_string.is_valid_hash_number s then
            Const_int (Ext_string.hash_number_as_i32_exn s)
          else Const_string {s; delim = None}
        in
        prim
          ~primitive:(Pmakeblock (info, mutable_flag))
          ~args:[Lam.const tag_val; value]
          loc
      | _ -> assert false))
  | Pfn_arity -> prim ~primitive:Pfn_arity ~args loc
  | Pdebugger -> prim ~primitive:Pdebugger ~args loc
  | Ptypeof -> prim ~primitive:Ptypeof ~args loc
  | Pisnullable -> prim ~primitive:Pis_null_undefined ~args loc
  | Pnull_to_opt -> prim ~primitive:Pnull_to_opt ~args loc
  | Pnullable_to_opt -> prim ~primitive:Pnull_undefined_to_opt ~args loc
  | Pis_not_none -> prim ~primitive:Pis_not_none ~args loc
  | Pval_from_option -> prim ~primitive:Pval_from_option ~args loc
  | Pval_from_option_not_nest ->
    prim ~primitive:Pval_from_option_not_nest ~args loc
  | Pjscomp x -> prim ~primitive:(Pjscomp x) ~args loc
  | Pfield (id, info) -> prim ~primitive:(Pfield (id, info)) ~args loc
  | Psetfield (id, info) -> prim ~primitive:(Psetfield (id, info)) ~args loc
  | Pduprecord -> prim ~primitive:Pduprecord ~args loc
  | Ptagged_template -> prim ~primitive:Ptagged_template ~args loc
  | Precord_rest excluded -> prim ~primitive:(Precord_rest excluded) ~args loc
  | Praise -> prim ~primitive:Praise ~args loc
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
  | Ppowint -> prim ~primitive:Ppowint ~args loc
  | Pandint -> prim ~primitive:Pandint ~args loc
  | Porint -> prim ~primitive:Porint ~args loc
  | Pxorint -> prim ~primitive:Pxorint ~args loc
  | Pnotint -> prim ~primitive:Pnotint ~args loc
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
  | Ppowfloat -> prim ~primitive:Ppowfloat ~args loc
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
  | Pnotbigint -> prim ~primitive:Pnotbigint ~args loc
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
  | Pmakearray -> prim ~primitive:Pmakearray ~args loc
  | Parraylength -> prim ~primitive:Parraylength ~args loc
  | Parrayrefu -> prim ~primitive:Parrayrefu ~args loc
  | Parraysetu -> prim ~primitive:Parraysetu ~args loc
  | Parrayrefs -> prim ~primitive:Parrayrefs ~args loc
  | Parraysets -> prim ~primitive:Parraysets ~args loc
  | Pmakelist -> prim ~primitive:Pmakelist ~args loc
  | Pmakedict -> prim ~primitive:Pmakedict ~args loc
  | Pdict_has -> prim ~primitive:Pdict_has ~args loc
  | Pawait -> prim ~primitive:Pawait ~args loc
  | Pimport src -> prim ~primitive:(Pimport src) ~args loc
  | Pinit_mod -> (
    match args with
    | [_loc; Lconst (Const_block (_, [Const_block (_, [])]))] -> Lam.unit
    | _ -> prim ~primitive:Pinit_mod ~args loc)
  | Pupdate_mod -> (
    match args with
    | [Lconst (Const_block (_, [Const_block (_, [])])); _; _] -> Lam.unit
    | _ -> prim ~primitive:Pupdate_mod ~args loc)
  | Phash -> prim ~primitive:Phash ~args loc
  | Phash_mixint -> prim ~primitive:Phash_mixint ~args loc
  | Phash_mixstring -> prim ~primitive:Phash_mixstring ~args loc
  | Phash_finalmix -> prim ~primitive:Phash_finalmix ~args loc
  | Pcurry_apply _ -> prim ~primitive:Pjs_apply ~args loc
  | Pis_poly_var_block -> prim ~primitive:Pis_poly_var_block ~args loc
  | Pjs_call {prim_name; arg_types; ffi; transformed_jsx} ->
    prim
      ~primitive:(Pjs_call {prim_name; arg_types; ffi; transformed_jsx})
      ~args loc
  | Pjs_object_create labels ->
    prim ~primitive:(Pjs_object_create labels) ~args loc
  | Pjs_object_get name -> prim ~primitive:(Pjs_object_get name) ~args loc
  | Pjs_object_set name -> prim ~primitive:(Pjs_object_set name) ~args loc
  | Praw_js_code info -> prim ~primitive:(Praw_js_code info) ~args loc
  | Pjs_fn_method -> prim ~primitive:Pjs_fn_method ~args loc

(* Does not exist since we compile array in js backend unlike native backend *)

let may_depend = Lam_module_ident.Hash_set.add

let convert (lam : Lambda.lambda) : Lam.t * Lam_module_ident.Hash_set.t =
  let may_depends = Lam_module_ident.Hash_set.create 0 in

  let rec convert_aux (lam : Lambda.lambda) : Lam.t =
    match lam with
    | Lvar x -> Lam.var x
    | Lconst x -> Lam.const (Lam_constant_convert.convert_constant x)
    | Lapply
        {
          ap_func = fn;
          ap_args = args;
          ap_loc = loc;
          ap_inlined;
          ap_transformed_jsx;
        } ->
      (* we need do this eargly in case [aux fn] add some wrapper *)
      Lam.apply (convert_aux fn)
        (Ext_list.map args convert_aux)
        {ap_loc = loc; ap_inlined; ap_status = App_uncurry}
        ~ap_transformed_jsx
    | Lfunction {params; body; attr; loc} ->
      Lam.function_ ~loc ~attr ~arity:(List.length params) ~params
        ~body:(convert_aux body)
    | Llet (kind, Pgenval, id, e, body) ->
      Lam.let_ kind id (convert_aux e) (convert_aux body)
    | Lletrec (bindings, body) ->
      Lam.letrec (Ext_list.map_snd bindings convert_aux) (convert_aux body)
    | Lprim (Pgetglobal id, args, _) ->
      let args = Ext_list.map args convert_aux in
      if Ident.is_predef_exn id then
        Lam.const (Const_string {s = id.name; delim = None})
      else (
        may_depend may_depends (Lam_module_ident.of_ml id);
        assert (args = []);
        Lam.global_module id)
    | Lprim (primitive, args, loc) ->
      let args = Ext_list.map args convert_aux in
      lam_prim ~primitive ~args loc
    | Lswitch (e, s, _loc) -> convert_switch e s
    | Lstringswitch (e, cases, default, _) ->
      Lam.stringswitch (convert_aux e)
        (Ext_list.map_snd cases convert_aux)
        (Ext_option.map default convert_aux)
    | Lstaticraise (id, args) ->
      Lam.staticraise id (Ext_list.map args convert_aux)
    | Lstaticcatch (b, (i, ids), handler) ->
      Lam.staticcatch (convert_aux b) (i, ids) (convert_aux handler)
    | Ltrywith (b, id, handler) ->
      Lam.try_ (convert_aux b) id (convert_aux handler)
    | Lifthenelse (b, then_, else_) ->
      Lam.if_ (convert_aux b) (convert_aux then_) (convert_aux else_)
    | Lsequence (a, b) -> Lam.seq (convert_aux a) (convert_aux b)
    | Lbreak -> Lam.break
    | Lcontinue -> Lam.continue
    | Lwhile (b, body) -> Lam.while_ (convert_aux b) (convert_aux body)
    | Lfor (id, from_, to_, dir, loop) ->
      Lam.for_ id (convert_aux from_) (convert_aux to_) dir (convert_aux loop)
    | Lfor_of (id, iterable, body) ->
      Lam.for_of id (convert_aux iterable) (convert_aux body)
    | Lfor_await_of (id, iterable, body) ->
      Lam.for_await_of id (convert_aux iterable) (convert_aux body)
    | Lassign (id, body) -> Lam.assign id (convert_aux body)
  and convert_switch (e : Lambda.lambda) (s : Lambda.lambda_switch) =
    Lam.switch (convert_aux e)
      {
        sw_consts_full = s.sw_consts_full;
        sw_consts = Ext_list.map_snd s.sw_consts convert_aux;
        sw_blocks_full = s.sw_blocks_full;
        sw_blocks = Ext_list.map_snd s.sw_blocks convert_aux;
        sw_failaction = Ext_option.map s.sw_failaction convert_aux;
        sw_dispatch = s.sw_dispatch;
      }
  in
  (convert_aux lam, may_depends)
