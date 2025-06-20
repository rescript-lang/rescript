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

type ident = Ident.t
type apply_status = App_na | App_infer_full | App_uncurry

type ap_info = {
  ap_loc: Location.t;
  ap_inlined: Lambda.inline_attribute;
  ap_status: apply_status;
}

module Types = struct
  type lambda_switch = {
    sw_consts_full: bool;
    (* TODO: refine its representation *)
    sw_consts: (int * t) list;
    sw_blocks_full: bool;
    sw_blocks: (int * t) list;
    sw_failaction: t option;
    sw_names: Ast_untagged_variants.switch_names option;
  }

  and lfunction = {
    arity: int;
    params: ident list;
    body: t;
    attr: Lambda.function_attribute;
  }

  (*
     Invariant:
     length (sw_consts) <= sw_consts_full
     when length (sw_consts) >= sw_consts_full -> true
     Note that failaction would appear in both
      {[
        match x with
        | ..
          | ..
            | _ -> 2
      ]}
      since compiler would first test [x] is a const pointer
      or not then the [default] applies to each branch.

      In most cases: {[
        let sw =
          {sw_consts_full = cstr.cstr_consts; sw_consts = consts;
           sw_blocks_full = cstr.cstr_nonconsts; sw_blocks = nonconsts;
           sw_failaction = None} in
      ]}

      but there are some edge cases (see https://caml.inria.fr/mantis/view.php?id=6033)
      one predicate used is
      {[
        (sw.sw_consts_full - List.length sw.sw_consts) +
        (sw.sw_blocks_full - List.length sw.sw_blocks) > 1
      ]}
      if [= 1] with [some fail] -- called once
      if [= 0] could not have [some fail]
  *)
  and prim_info = {primitive: Lam_primitive.t; args: t list; loc: Location.t}

  and apply = {
    ap_func: t;
    ap_args: t list;
    ap_info: ap_info;
    ap_transformed_jsx: bool;
  }

  and t =
    | Lvar of ident
    | Lglobal_module of ident * bool
    | Lconst of Lam_constant.t
    | Lapply of apply
    | Lfunction of lfunction
    | Llet of Lam_compat.let_kind * ident * t * t
    | Lletrec of (ident * t) list * t
    | Lprim of prim_info
    | Lswitch of t * lambda_switch
    | Lstringswitch of t * (string * t) list * t option
    | Lstaticraise of int * t list
    | Lstaticcatch of t * (int * ident list) * t
    | Ltrywith of t * ident * t
    | Lifthenelse of t * t * t
    | Lsequence of t * t
    | Lwhile of t * t
    | Lfor of ident * t * t * Asttypes.direction_flag * t
    | Lassign of ident * t
  (* | Lsend of Lam_compat.meth_kind * t * t * t list * Location.t *)
end

module X = struct
  type lambda_switch = Types.lambda_switch = {
    sw_consts_full: bool;
    sw_consts: (int * t) list;
    sw_blocks_full: bool;
    sw_blocks: (int * t) list;
    sw_failaction: t option;
    sw_names: Ast_untagged_variants.switch_names option;
  }

  and prim_info = Types.prim_info = {
    primitive: Lam_primitive.t;
    args: t list;
    loc: Location.t;
  }

  and apply = Types.apply = {
    ap_func: t;
    ap_args: t list;
    ap_info: ap_info;
    ap_transformed_jsx: bool;
  }

  and lfunction = Types.lfunction = {
    arity: int;
    params: ident list;
    body: t;
    attr: Lambda.function_attribute;
  }

  and t = Types.t =
    | Lvar of ident
    | Lglobal_module of ident * bool
    | Lconst of Lam_constant.t
    | Lapply of apply
    | Lfunction of lfunction
    | Llet of Lam_compat.let_kind * ident * t * t
    | Lletrec of (ident * t) list * t
    | Lprim of prim_info
    | Lswitch of t * lambda_switch
    | Lstringswitch of t * (string * t) list * t option
    | Lstaticraise of int * t list
    | Lstaticcatch of t * (int * ident list) * t
    | Ltrywith of t * ident * t
    | Lifthenelse of t * t * t
    | Lsequence of t * t
    | Lwhile of t * t
    | Lfor of ident * t * t * Asttypes.direction_flag * t
    | Lassign of ident * t
  (* | Lsend of Lam_compat.meth_kind * t * t * t list * Location.t *)
end

include Types

(** apply [f] to direct successor which has type [Lam.t] *)

let inner_map (l : t) (f : t -> X.t) : X.t =
  match l with
  | Lvar (_ : ident) | Lconst (_ : Lam_constant.t) -> ((* Obj.magic *) l : X.t)
  | Lapply {ap_func; ap_args; ap_info; ap_transformed_jsx} ->
    let ap_func = f ap_func in
    let ap_args = Ext_list.map ap_args f in
    Lapply {ap_func; ap_args; ap_info; ap_transformed_jsx}
  | Lfunction {body; arity; params; attr} ->
    let body = f body in
    Lfunction {body; arity; params; attr}
  | Llet (str, id, arg, body) ->
    let arg = f arg in
    let body = f body in
    Llet (str, id, arg, body)
  | Lletrec (decl, body) ->
    let body = f body in
    let decl = Ext_list.map_snd decl f in
    Lletrec (decl, body)
  | Lglobal_module _ -> (l : X.t)
  | Lprim {args; primitive; loc} ->
    let args = Ext_list.map args f in
    Lprim {args; primitive; loc}
  | Lswitch
      ( arg,
        {
          sw_consts;
          sw_consts_full;
          sw_blocks;
          sw_blocks_full;
          sw_failaction;
          sw_names;
        } ) ->
    let arg = f arg in
    let sw_consts = Ext_list.map_snd sw_consts f in
    let sw_blocks = Ext_list.map_snd sw_blocks f in
    let sw_failaction = Ext_option.map sw_failaction f in
    Lswitch
      ( arg,
        {
          sw_consts;
          sw_blocks;
          sw_failaction;
          sw_blocks_full;
          sw_consts_full;
          sw_names;
        } )
  | Lstringswitch (arg, cases, default) ->
    let arg = f arg in
    let cases = Ext_list.map_snd cases f in
    let default = Ext_option.map default f in
    Lstringswitch (arg, cases, default)
  | Lstaticraise (id, args) ->
    let args = Ext_list.map args f in
    Lstaticraise (id, args)
  | Lstaticcatch (e1, vars, e2) ->
    let e1 = f e1 in
    let e2 = f e2 in
    Lstaticcatch (e1, vars, e2)
  | Ltrywith (e1, exn, e2) ->
    let e1 = f e1 in
    let e2 = f e2 in
    Ltrywith (e1, exn, e2)
  | Lifthenelse (e1, e2, e3) ->
    let e1 = f e1 in
    let e2 = f e2 in
    let e3 = f e3 in
    Lifthenelse (e1, e2, e3)
  | Lsequence (e1, e2) ->
    let e1 = f e1 in
    let e2 = f e2 in
    Lsequence (e1, e2)
  | Lwhile (e1, e2) ->
    let e1 = f e1 in
    let e2 = f e2 in
    Lwhile (e1, e2)
  | Lfor (v, e1, e2, dir, e3) ->
    let e1 = f e1 in
    let e2 = f e2 in
    let e3 = f e3 in
    Lfor (v, e1, e2, dir, e3)
  | Lassign (id, e) ->
    let e = f e in
    Lassign (id, e)
(* | Lsend (k, met, obj, args, loc) ->
   let met = f met in
   let obj = f obj in
   let args = Ext_list.map args f in
   Lsend(k,met,obj,args,loc) *)

exception Not_simple_form

(**


   [is_eta_conversion_exn params inner_args outer_args]
   case 1:
   {{
    (fun params -> wrap (primitive (inner_args)) args
   }}
   when [inner_args] are the same as [params], it can be simplified as
   [wrap (primitive args)]

    where [wrap] used to be simple instructions
    Note that [external] functions are forced to do eta-conversion
    when combined with [|>] operator, we need to make sure beta-reduction
    is applied though since `[@variadic]` needs such guarantee.
    Since `[@variadic] is the tail position
*)
let rec is_eta_conversion_exn params inner_args outer_args : t list =
  match (params, inner_args, outer_args) with
  | x :: xs, Lvar y :: ys, r :: rest when Ident.same x y ->
    r :: is_eta_conversion_exn xs ys rest
  | ( x :: xs,
      Lprim
        ({primitive = Pjs_fn_make _ | Pjs_fn_make_unit; args = [Lvar y]} as p)
      :: ys,
      r :: rest )
    when Ident.same x y ->
    Lprim {p with args = [r]} :: is_eta_conversion_exn xs ys rest
  | [], [], [] -> []
  | _, _, _ -> raise_notrace Not_simple_form

(** FIXME: more robust inlining check later, we should inline it before we add stub code*)
let rec apply ?(ap_transformed_jsx = false) fn args (ap_info : ap_info) : t =
  match fn with
  | Lfunction
      {
        params;
        body =
          Lprim
            {
              primitive =
                ( Pnull_to_opt | Pnull_undefined_to_opt | Pis_null
                | Pis_null_undefined | Ptypeof ) as wrap;
              args =
                [Lprim ({primitive = _; args = inner_args} as primitive_call)];
            };
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args ->
      let loc = ap_info.ap_loc in
      Lprim
        {primitive = wrap; args = [Lprim {primitive_call with args; loc}]; loc}
    | exception Not_simple_form ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx})
  | Lfunction
      {
        params;
        body = Lprim ({primitive = _; args = inner_args} as primitive_call);
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args -> Lprim {primitive_call with args; loc = ap_info.ap_loc}
    | exception _ ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx})
  | Lfunction
      {
        params;
        body =
          Lsequence
            ( Lprim ({primitive = _; args = inner_args} as primitive_call),
              (Lconst _ as const) );
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args ->
      Lsequence (Lprim {primitive_call with args; loc = ap_info.ap_loc}, const)
    | exception _ ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx}
      (* | Lfunction {params;body} when Ext_list.same_length params args ->
          Ext_list.fold_right2 (fun p arg acc ->
            Llet(Strict,p,arg,acc)
          ) params args body *)
      (* TODO: more rigirous analysis on [let_kind] *))
  | Llet (kind, id, e, (Lfunction _ as fn)) ->
    Llet (kind, id, e, apply fn args ap_info ~ap_transformed_jsx)
  (* | Llet (kind0, id0, e0, Llet (kind,id, e, (Lfunction _ as fn))) ->
     Llet(kind0,id0,e0,Llet (kind, id, e, apply fn args loc status)) *)
  | _ -> Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx}

let rec eq_approx (l1 : t) (l2 : t) =
  match l1 with
  | Lglobal_module (i1, b1) -> (
    match l2 with
    | Lglobal_module (i2, b2) -> Ident.same i1 i2 && b1 = b2
    | _ -> false)
  | Lvar i1 -> (
    match l2 with
    | Lvar i2 -> Ident.same i1 i2
    | _ -> false)
  | Lconst c1 -> (
    match l2 with
    | Lconst c2 -> Lam_constant.eq_approx c1 c2
    | _ -> false)
  | Lapply app1 -> (
    match l2 with
    | Lapply app2 ->
      eq_approx app1.ap_func app2.ap_func
      && eq_approx_list app1.ap_args app2.ap_args
    | _ -> false)
  | Lifthenelse (a, b, c) -> (
    match l2 with
    | Lifthenelse (a0, b0, c0) ->
      eq_approx a a0 && eq_approx b b0 && eq_approx c c0
    | _ -> false)
  | Lsequence (a, b) -> (
    match l2 with
    | Lsequence (a0, b0) -> eq_approx a a0 && eq_approx b b0
    | _ -> false)
  | Lwhile (p, b) -> (
    match l2 with
    | Lwhile (p0, b0) -> eq_approx p p0 && eq_approx b b0
    | _ -> false)
  | Lassign (v0, l0) -> (
    match l2 with
    | Lassign (v1, l1) -> Ident.same v0 v1 && eq_approx l0 l1
    | _ -> false)
  | Lstaticraise (id, ls) -> (
    match l2 with
    | Lstaticraise (id1, ls1) -> id = id1 && eq_approx_list ls ls1
    | _ -> false)
  | Lprim info1 -> (
    match l2 with
    | Lprim info2 ->
      Lam_primitive.eq_primitive_approx info1.primitive info2.primitive
      && eq_approx_list info1.args info2.args
    | _ -> false)
  | Lstringswitch (arg, patterns, default) -> (
    match l2 with
    | Lstringswitch (arg2, patterns2, default2) ->
      eq_approx arg arg2 && eq_option default default2
      && Ext_list.for_all2_no_exn patterns patterns2
           (fun ((k : string), v) (k2, v2) -> k = k2 && eq_approx v v2)
    | _ -> false)
  | Lfunction _
  | Llet (_, _, _, _)
  | Lletrec _ | Lswitch _ | Lstaticcatch _ | Ltrywith _
  | Lfor (_, _, _, _, _) ->
    false

and eq_option l1 l2 =
  match l1 with
  | None -> l2 = None
  | Some l1 -> (
    match l2 with
    | Some l2 -> eq_approx l1 l2
    | None -> false)

and eq_approx_list ls ls1 = Ext_list.for_all2_no_exn ls ls1 eq_approx

let switch lam (lam_switch : lambda_switch) : t =
  match lam with
  | Lconst (Const_int {i}) -> (
    (* Because of inlining and dead code, we might be looking at a value of unexpected type
       e.g. an integer, so the const case might not be found *)
    try
      Ext_list.assoc_by_int lam_switch.sw_consts (Int32.to_int i)
        lam_switch.sw_failaction
    with _ -> Lswitch (lam, lam_switch))
  | Lconst (Const_block (i, _, _)) -> (
    try Ext_list.assoc_by_int lam_switch.sw_blocks i lam_switch.sw_failaction
    with _ -> Lswitch (lam, lam_switch))
  | _ -> Lswitch (lam, lam_switch)

let stringswitch (lam : t) cases default : t =
  match lam with
  | Lconst (Const_string {s; unicode = false}) ->
    Ext_list.assoc_by_string cases s default
  | _ -> Lstringswitch (lam, cases, default)

let true_ : t = Lconst Const_js_true
let false_ : t = Lconst Const_js_false
let unit : t = Lconst (Const_js_undefined {is_unit = true})

let rec seq (a : t) b : t =
  match a with
  | Lprim {primitive = Pmakeblock _; args = x :: xs} ->
    seq (Ext_list.fold_left xs x seq) b
  | Lprim {primitive = Pnull_to_opt | Pnull_undefined_to_opt; args = [a]} ->
    seq a b
  | _ -> Lsequence (a, b)

let var id : t = Lvar id
let global_module ?(dynamic_import = false) id =
  Lglobal_module (id, dynamic_import)
let const ct : t = Lconst ct

let function_ ~attr ~arity ~params ~body : t =
  Lfunction {arity; params; body; attr}

let let_ kind id e body : t = Llet (kind, id, e, body)
let letrec bindings body : t = Lletrec (bindings, body)
let while_ a b : t = Lwhile (a, b)
let try_ body id handler : t = Ltrywith (body, id, handler)
let for_ v e1 e2 dir e3 : t = Lfor (v, e1, e2, dir, e3)
let assign v l : t = Lassign (v, l)
let staticcatch a b c : t = Lstaticcatch (a, b, c)
let staticraise a b : t = Lstaticraise (a, b)

module Lift = struct
  let int i : t = Lconst (Const_int {i; comment = None})

  (* let int32 i : t =
     Lconst ((Const_int32 i)) *)

  let bool b = if b then true_ else false_

  let string s : t = Lconst (Const_string {s; unicode = false})

  let char b : t = Lconst (Const_char b)
end

let prim ~primitive:(prim : Lam_primitive.t) ~args loc : t =
  let default () : t = Lprim {primitive = prim; args; loc} in
  match args with
  | [Lconst a] -> (
    match (prim, a) with
    | Pnegint, Const_int {i} -> Lift.int (Int32.neg i)
    (* | Pfloatofint, ( (Const_int a)) *)
    (*   -> Lift.float (float_of_int a) *)
    | Pintoffloat, Const_float a ->
      Lift.int (Int32.of_float (float_of_string a))
    (* | Pnegfloat -> Lift.float (-. a) *)
    (* | Pabsfloat -> Lift.float (abs_float a) *)
    | Pstringlength, Const_string {s; unicode = false} ->
      Lift.int (Int32.of_int (String.length s))
    (* | Pnegbint Pnativeint, ( (Const_nativeint i)) *)
    (*   ->   *)
    (*   Lift.nativeint (Nativeint.neg i) *)
    | Pnot, Const_js_true -> false_
    | Pnot, Const_js_false -> true_
    | _ -> default ())
  | [Lconst a; Lconst b] -> (
    match (prim, a, b) with
    | Pintcomp cmp, Const_int a, Const_int b ->
      Lift.bool (Lam_compat.cmp_int32 cmp a.i b.i)
    | Pfloatcomp cmp, Const_float a, Const_float b ->
      (* FIXME: could raise? *)
      Lift.bool
        (Lam_compat.cmp_float cmp (float_of_string a) (float_of_string b))
    | Pbigintcomp cmp, Const_bigint _, Const_bigint _ -> default ()
    | Pintcomp ((Ceq | Cneq) as op), Const_pointer a, Const_pointer b ->
      Lift.bool
        (match op with
        | Ceq -> a = (b : string)
        | Cneq -> a <> b
        | _ -> assert false)
    | ( ( Paddint | Psubint | Pmulint | Pdivint | Pmodint | Pandint | Porint
        | Pxorint | Plslint | Plsrint | Pasrint ),
        Const_int {i = aa},
        Const_int {i = bb} ) -> (
      (* WE SHOULD keep it as [int], to preserve types *)
      let int_ = Lift.int in
      match prim with
      | Paddint -> int_ (Int32.add aa bb)
      | Psubint -> int_ (Int32.sub aa bb)
      | Pmulint -> int_ (Int32.mul aa bb)
      | Pdivint -> if bb = 0l then default () else int_ (Int32.div aa bb)
      | Pmodint -> if bb = 0l then default () else int_ (Int32.rem aa bb)
      | Pandint -> int_ (Int32.logand aa bb)
      | Porint -> int_ (Int32.logor aa bb)
      | Pxorint -> int_ (Int32.logxor aa bb)
      | Plslint -> int_ (Int32.shift_left aa (Int32.to_int bb))
      | Plsrint -> int_ (Int32.shift_right_logical aa (Int32.to_int bb))
      | Pasrint -> int_ (Int32.shift_right aa (Int32.to_int bb))
      | _ -> default ())
    | Psequand, Const_js_false, (Const_js_true | Const_js_false) -> false_
    | Psequand, Const_js_true, Const_js_true -> true_
    | Psequand, Const_js_true, Const_js_false -> false_
    | Psequor, Const_js_true, (Const_js_true | Const_js_false) -> true_
    | Psequor, Const_js_false, Const_js_true -> true_
    | Psequor, Const_js_false, Const_js_false -> false_
    | ( Pstringadd,
        Const_string {s = a; unicode = false},
        Const_string {s = b; unicode = false} ) ->
      Lift.string (a ^ b)
    | ( (Pstringrefs | Pstringrefu),
        Const_string {s = a; unicode = false},
        Const_int {i = b} ) -> (
      try Lift.char (Char.code (String.get a (Int32.to_int b)))
      with _ -> default ())
    | _ -> default ())
  | _ -> (
    match prim with
    | Pmakeblock (_size, Blk_module fields, _) -> (
      let rec aux fields args (var : Ident.t) i =
        match (fields, args) with
        | [], [] -> true
        | ( f :: fields,
            Lprim
              {
                primitive = Pfield (pos, Fld_module {name = f1});
                args = [(Lglobal_module (v1, _) | Lvar v1)];
              }
            :: args ) ->
          pos = i && f = f1 && Ident.same var v1 && aux fields args var (i + 1)
        | _, _ -> false
      in
      match (fields, args) with
      | ( field1 :: rest,
          Lprim
            {
              primitive = Pfield (pos, Fld_module {name = f1});
              args = [((Lglobal_module (v1, _) | Lvar v1) as lam)];
            }
          :: args1 ) ->
        if pos = 0 && field1 = f1 && aux rest args1 v1 1 then lam
        else default ()
      | _ -> default ())
    (* In this level, include is already expanded, so that
       {[
         { x0 : y0 ; x1 : y1 }
       ]}
       such module x can indeed be replaced by module y
    *)
    | _ -> default ())

let not_ loc x : t =
  match x with
  | Lprim ({primitive = Pintcomp Cneq} as prim) ->
    Lprim {prim with primitive = Pintcomp Ceq}
  | _ -> prim ~primitive:Pnot ~args:[x] loc

let has_boolean_type (x : t) =
  match x with
  | Lprim
      {
        primitive =
          ( Pnot | Psequand | Psequor | Pisout _ | Pis_not_none | Pobjcomp _
          | Pboolcomp _ | Pintcomp _ | Pfloatcomp _ | Pbigintcomp _
          | Pstringcomp _ );
        loc;
      } ->
    Some loc
  | _ -> None

(** [complete_range sw_consts 0 7]
    is complete with [0,1,.. 7]
*)
let rec complete_range (sw_consts : (int * _) list) ~(start : int) ~finish =
  match sw_consts with
  | [] -> finish < start
  | (i, _) :: rest ->
    start <= finish && i = start
    && complete_range rest ~start:(start + 1) ~finish

let rec eval_const_as_bool (v : Lam_constant.t) : bool =
  match v with
  | Const_int {i = x} -> x <> 0l
  | Const_char x -> x <> 0
  | Const_js_false | Const_js_null | Const_module_alias | Const_js_undefined _
    ->
    false
  | Const_js_true | Const_string _ | Const_pointer _ | Const_float _
  | Const_bigint _ | Const_block _ ->
    true
  | Const_some b -> eval_const_as_bool b

let if_ (a : t) (b : t) (c : t) : t =
  match a with
  | Lconst v -> if eval_const_as_bool v then b else c
  | _ -> (
    match (b, c) with
    | _, Lconst (Const_int {comment = Pt_assertfalse}) ->
      seq a b (* TODO: we could customize more cases *)
    | Lconst (Const_int {comment = Pt_assertfalse}), _ -> seq a c
    | Lconst Const_js_true, Lconst Const_js_false ->
      if has_boolean_type a != None then a else Lifthenelse (a, b, c)
    | Lconst Const_js_false, Lconst Const_js_true -> (
      match has_boolean_type a with
      | Some loc -> not_ loc a
      | None -> Lifthenelse (a, b, c))
    | Lprim {primitive = Praise}, _ -> (
      match c with
      | Lconst _ -> Lifthenelse (a, b, c)
      | _ -> seq (Lifthenelse (a, b, unit)) c)
    | _ -> (
      match a with
      | Lprim
          {
            primitive = Pisout off;
            args = [Lconst (Const_int {i = range}); Lvar xx];
          } -> (
        let range = Int32.to_int range in
        match c with
        | Lswitch
            ( (Lvar yy as switch_arg),
              ({
                 sw_blocks = [];
                 sw_blocks_full = true;
                 sw_consts;
                 sw_consts_full = _;
                 sw_failaction = None;
               } as body) )
          when Ident.same xx yy
               && complete_range sw_consts ~start:(-off) ~finish:(range - off)
          ->
          Lswitch
            ( switch_arg,
              {body with sw_failaction = Some b; sw_consts_full = false} )
        | _ -> Lifthenelse (a, b, c))
      | Lprim {primitive = Pisint; args = [Lvar i]; _} -> (
        match b with
        | Lifthenelse
            (Lprim {primitive = Pintcomp Ceq; args = [Lvar j; Lconst _]}, _, b_f)
          when Ident.same i j && eq_approx b_f c ->
          b
        | Lprim {primitive = Pintcomp Ceq; args = [Lvar j; Lconst _]}
          when Ident.same i j && eq_approx false_ c ->
          b
        | Lifthenelse
            ( Lprim
                ({primitive = Pintcomp Cneq; args = [Lvar j; Lconst _]} as
                 b_pred),
              b_t,
              b_f )
          when Ident.same i j && eq_approx b_t c ->
          Lifthenelse (Lprim {b_pred with primitive = Pintcomp Ceq}, b_f, b_t)
        | Lprim
            {primitive = Pintcomp Cneq; args = [Lvar j; Lconst _] as args; loc}
        | Lprim
            {
              primitive = Pnot;
              args =
                [
                  Lprim
                    {
                      primitive = Pintcomp Ceq;
                      args = [Lvar j; Lconst _] as args;
                      loc;
                    };
                ];
            }
          when Ident.same i j && eq_approx true_ c ->
          Lprim {primitive = Pintcomp Cneq; args; loc}
        | _ -> Lifthenelse (a, b, c))
      | _ -> Lifthenelse (a, b, c)))

(* TODO: the smart constructor is not exploited yet*)
(* [l || r ] *)
let sequor l r = if_ l true_ r

(** [l && r ] *)
let sequand l r = if_ l r false_

let result_wrap loc (result_type : External_ffi_types.return_wrapper) result =
  match result_type with
  | Return_replaced_with_unit -> seq result unit
  | Return_null_to_opt -> prim ~primitive:Pnull_to_opt ~args:[result] loc
  | Return_null_undefined_to_opt ->
    prim ~primitive:Pnull_undefined_to_opt ~args:[result] loc
  | Return_unset | Return_identity -> result

let handle_bs_non_obj_ffi ?(transformed_jsx = false)
    (arg_types : External_arg_spec.params)
    (result_type : External_ffi_types.return_wrapper) ffi args loc prim_name
    ~dynamic_import =
  result_wrap loc result_type
    (prim
       ~primitive:
         (Pjs_call {prim_name; arg_types; ffi; dynamic_import; transformed_jsx})
       ~args loc)
