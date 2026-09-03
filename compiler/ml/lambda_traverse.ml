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

(* Generic walks over a Lambda term: which variables are free, substituting
   for them, and the canonical key two terms are compared by. Each builds only
   through Lambda's constructors, so what comes out is normalized like any
   other term. *)

open Lambda

(** [shallow_map_sharing f lam] rewrites [lam]'s immediate children with [f]
    and rebuilds the node through its smart constructor, so the result is
    normalized. A node whose children all come back physically unchanged is
    returned as-is, so a traversal that rewrites nothing allocates nothing. *)
let shallow_map_sharing (f : t -> t) (lam : t) : t =
  match lam with
  | Lvar _ | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> lam
  | Lapply ap ->
    let fn = f ap.ap_func in
    let args = Ext_list.map_sharing ap.ap_args f in
    if fn == ap.ap_func && args == ap.ap_args then lam
    else apply fn args ap.ap_info ~ap_transformed_jsx:ap.ap_transformed_jsx
  | Lfunction {params; body; attr; loc} ->
    let body' = f body in
    if body' == body then lam else function_ ~loc ~attr ~params ~body:body'
  | Llet (k, id, e, b) ->
    let e' = f e and b' = f b in
    if e' == e && b' == b then lam else let_ k id e' b'
  | Lletrec (bs, b) ->
    let bs' = Ext_list.map_snd_sharing bs f and b' = f b in
    if bs' == bs && b' == b then lam else letrec bs' b'
  | Lprim {primitive; args; loc} ->
    let args' = Ext_list.map_sharing args f in
    if args' == args then lam else prim ~primitive ~args:args' loc
  | Lswitch (e, sw) ->
    let e' = f e in
    let consts = Ext_list.map_snd_sharing sw.sw_consts f in
    let blocks = Ext_list.map_snd_sharing sw.sw_blocks f in
    let fail = Ext_option.map_sharing sw.sw_failaction f in
    if
      e' == e && consts == sw.sw_consts && blocks == sw.sw_blocks
      && fail == sw.sw_failaction
    then lam
    else
      switch e'
        {sw with sw_consts = consts; sw_blocks = blocks; sw_failaction = fail}
  | Lstringswitch (e, cases, d) ->
    let e' = f e in
    let cases' = Ext_list.map_snd_sharing cases f in
    let d' = Ext_option.map_sharing d f in
    if e' == e && cases' == cases && d' == d then lam
    else stringswitch e' cases' d'
  | Lstaticraise (i, args) ->
    let args' = Ext_list.map_sharing args f in
    if args' == args then lam else staticraise i args'
  | Lstaticcatch (b, h, hd) ->
    let b' = f b and hd' = f hd in
    if b' == b && hd' == hd then lam else staticcatch b' h hd'
  | Ltrywith (b, id, h) ->
    let b' = f b and h' = f h in
    if b' == b && h' == h then lam else try_ b' id h'
  | Lifthenelse (a, b, c) ->
    let a' = f a and b' = f b and c' = f c in
    if a' == a && b' == b && c' == c then lam else if_ a' b' c'
  | Lsequence (a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else seq a' b'
  | Lwhile (a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else while_ a' b'
  | Lfor (id, a, b, d, c) ->
    let a' = f a and b' = f b and c' = f c in
    if a' == a && b' == b && c' == c then lam else for_ id a' b' d c'
  | Lfor_of (id, a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else for_of id a' b'
  | Lfor_await_of (id, a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else for_await_of id a' b'
  | Lassign (id, b) ->
    let b' = f b in
    if b' == b then lam else assign id b'

(*
   Those keys are later compared with Pervasives.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

let make_key e =
  let count = ref 0 (* Used for controling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count;
    if !count > max_raw then raise_notrace Not_simple;
    (* Too big ! *)
    match e with
    | Lvar id -> ( try Ident.find_same id env with Not_found -> e)
    | Lglobal_module _ | Lconst _ -> e
    | Lapply ap ->
      apply ~ap_transformed_jsx:ap.ap_transformed_jsx (tr_rec env ap.ap_func)
        (tr_recs env ap.ap_args)
        {ap.ap_info with ap_loc = Location.none}
    | Llet (Alias, x, ex, e) ->
      (* Ignore aliases -> substitute *)
      let ex = tr_rec env ex in
      tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt), x, ex, Lvar v) when Ident.same v x ->
      tr_rec env ex
    | Llet (str, x, ex, e) ->
      (* Because of side effects, keep other lets with normalized names *)
      let ex = tr_rec env ex in
      let y = make_key x in
      let_ str y ex (tr_rec (Ident.add x (var y) env) e)
    | Lprim {primitive = p; args = es; loc = _} ->
      prim ~primitive:p ~args:(tr_recs env es) Location.none
    | Lswitch (e, sw) -> switch (tr_rec env e) (tr_sw env sw)
    | Lstringswitch (e, sw, d) ->
      stringswitch (tr_rec env e)
        (List.map (fun (s, e) -> (s, tr_rec env e)) sw)
        (tr_opt env d)
    | Lstaticraise (i, es) -> staticraise i (tr_recs env es)
    | Lstaticcatch (e1, xs, e2) ->
      staticcatch (tr_rec env e1) xs (tr_rec env e2)
    | Ltrywith (e1, x, e2) -> try_ (tr_rec env e1) x (tr_rec env e2)
    | Lifthenelse (cond, ifso, ifnot) ->
      if_ (tr_rec env cond) (tr_rec env ifso) (tr_rec env ifnot)
    | Lsequence (e1, e2) -> seq (tr_rec env e1) (tr_rec env e2)
    | Lbreak -> break
    | Lcontinue -> continue
    | Lassign (x, e) -> assign x (tr_rec env e)
    | Lletrec _ | Lfunction _ | Lfor _ | Lfor_of _ | Lfor_await_of _ | Lwhile _
      ->
      raise_notrace Not_simple
  and tr_recs env es = List.map (tr_rec env) es
  and tr_sw env sw =
    {
      sw with
      sw_consts = List.map (fun (i, e) -> (i, tr_rec env e)) sw.sw_consts;
      sw_blocks = List.map (fun (i, e) -> (i, tr_rec env e)) sw.sw_blocks;
      sw_failaction = tr_opt env sw.sw_failaction;
    }
  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e)
  in

  try Some (tr_rec Ident.empty e) with Not_simple -> None

(* Does any immediate child satisfy [f]? Short-circuits. *)
let shallow_exists (f : t -> bool) (lam : t) : bool =
  match lam with
  | Lvar _ | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> false
  | Lapply {ap_func; ap_args} -> f ap_func || Ext_list.exists ap_args f
  | Lfunction {body} -> f body
  | Llet (_, _, arg, body) -> f arg || f body
  | Lletrec (decl, body) -> f body || Ext_list.exists_snd decl f
  | Lprim {args} -> Ext_list.exists args f
  | Lswitch (arg, {sw_consts; sw_blocks; sw_failaction}) ->
    f arg
    || Ext_list.exists_snd sw_consts f
    || Ext_list.exists_snd sw_blocks f
    || Ext_option.exists sw_failaction f
  | Lstringswitch (arg, cases, default) ->
    f arg || Ext_list.exists_snd cases f || Ext_option.exists default f
  | Lstaticraise (_, args) -> Ext_list.exists args f
  | Lstaticcatch (e1, _, e2) -> f e1 || f e2
  | Ltrywith (e1, _, e2) -> f e1 || f e2
  | Lifthenelse (e1, e2, e3) -> f e1 || f e2 || f e3
  | Lsequence (e1, e2) -> f e1 || f e2
  | Lwhile (e1, e2) -> f e1 || f e2
  | Lfor (_, e1, e2, _, e3) -> f e1 || f e2 || f e3
  | Lfor_of (_, e1, e2) | Lfor_await_of (_, e1, e2) -> f e1 || f e2
  | Lassign (_, e) -> f e

let iter f lam =
  ignore
    (shallow_exists
       (fun x ->
         f x;
         false)
       lam)

let free_ids get l =
  let fv = ref Set_ident.empty in
  let rec free l =
    iter free l;
    fv := List.fold_left Set_ident.add !fv (get l);
    match l with
    | Lfunction {params} ->
      List.iter (fun param -> fv := Set_ident.remove !fv param) params
    | Llet (_str, id, _arg, _body) -> fv := Set_ident.remove !fv id
    | Lletrec (decl, _body) ->
      List.iter (fun (id, _exp) -> fv := Set_ident.remove !fv id) decl
    | Lstaticcatch (_e1, (_, vars), _e2) ->
      List.iter (fun id -> fv := Set_ident.remove !fv id) vars
    | Ltrywith (_e1, exn, _e2) -> fv := Set_ident.remove !fv exn
    | Lfor (v, _e1, _e2, _dir, _e3) -> fv := Set_ident.remove !fv v
    | Lfor_of (v, _e1, _e2) | Lfor_await_of (v, _e1, _e2) ->
      fv := Set_ident.remove !fv v
    | Lassign (id, _e) -> fv := Set_ident.add !fv id
    | Lvar _ | Lglobal_module _ | Lconst _ | Lapply _ | Lprim _ | Lswitch _
    | Lstringswitch _ | Lstaticraise _ | Lifthenelse _ | Lsequence _ | Lbreak
    | Lcontinue | Lwhile _ ->
      ()
  in
  free l;
  !fv

let free_variables l =
  free_ids
    (function
      | Lvar id -> [id]
      | _ -> [])
    l

(* Substitution rebuilds through [shallow_map_sharing], so the result is
   normalized and an untouched subterm is returned physically unchanged. *)
let subst_lambda s lam =
  let rec subst l =
    match l with
    | Lvar id -> ( try Ident.find_same id s with Not_found -> l)
    | _ -> shallow_map_sharing subst l
  in
  subst lam
