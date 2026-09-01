(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(* Adapted for Javascript backend : Hongbo Zhang,  *)

exception Real_reference

let rec eliminate_ref id (lam : Lambda.t) =
  match lam with
  (* we can do better escape analysis in Javascript backend *)
  | Lvar v -> if Ident.same v id then raise_notrace Real_reference else lam
  | Lprim {primitive = Pfield (0, _); args = [Lvar v]} when Ident.same v id ->
    Lambda.var id
  | Lfunction _ ->
    if Lam_hit.hit_variable id lam then raise_notrace Real_reference else lam
  (* In Javascript backend, its okay, we can reify it later
     a failed case
     {[
       for i = ..
           let v = ref 0
               for j = ..
                   incr v
                     a[j] = ()=>{!v}

     ]}
     here v is captured by a block, and it's a loop mutable value,
     we have to generate
     {[
       for i = ..
           let v = ref 0
               (function (v){for j = ..
                                   a[j] = ()=>{!v}}(v)

     ]}
     now, v is a real reference
     TODO: we can refine analysis in later
  *)
  (* Lfunction(kind, params, eliminate_ref id body) *)
  | Lprim {primitive = Psetfield (0, _); args = [Lvar v; e]}
    when Ident.same v id ->
    Lambda.assign id (eliminate_ref id e)
  | Lconst _ -> lam
  | Lapply {ap_func = e1; ap_args = el; ap_info; ap_transformed_jsx} ->
    Lambda.apply ~ap_transformed_jsx (eliminate_ref id e1)
      (Ext_list.map el (eliminate_ref id))
      ap_info
  | Llet (str, v, e1, e2) ->
    Lambda.let_ str v (eliminate_ref id e1) (eliminate_ref id e2)
  | Lletrec (idel, e2) ->
    Lambda.letrec
      (Ext_list.map idel (fun (v, e) -> (v, eliminate_ref id e)))
      (eliminate_ref id e2)
  | Lglobal_module _ -> lam
  | Lprim {primitive; args; loc} ->
    Lambda.prim ~primitive ~args:(Ext_list.map args (eliminate_ref id)) loc
  | Lswitch (e, sw) ->
    Lambda.switch (eliminate_ref id e)
      {
        sw_consts_full = sw.sw_consts_full;
        sw_consts =
          Ext_list.map sw.sw_consts (fun (n, e) -> (n, eliminate_ref id e));
        sw_blocks_full = sw.sw_blocks_full;
        sw_blocks =
          Ext_list.map sw.sw_blocks (fun (n, e) -> (n, eliminate_ref id e));
        sw_failaction =
          (match sw.sw_failaction with
          | None -> None
          | Some x -> Some (eliminate_ref id x));
        sw_dispatch = sw.sw_dispatch;
      }
  | Lstringswitch (e, sw, default) ->
    Lambda.stringswitch (eliminate_ref id e)
      (Ext_list.map sw (fun (s, e) -> (s, eliminate_ref id e)))
      (match default with
      | None -> None
      | Some x -> Some (eliminate_ref id x))
  | Lstaticraise (i, args) ->
    Lambda.staticraise i (Ext_list.map args (eliminate_ref id))
  | Lstaticcatch (e1, i, e2) ->
    Lambda.staticcatch (eliminate_ref id e1) i (eliminate_ref id e2)
  | Ltrywith (e1, v, e2) ->
    Lambda.try_ (eliminate_ref id e1) v (eliminate_ref id e2)
  | Lifthenelse (e1, e2, e3) ->
    Lambda.if_ (eliminate_ref id e1) (eliminate_ref id e2) (eliminate_ref id e3)
  | Lsequence (e1, e2) -> Lambda.seq (eliminate_ref id e1) (eliminate_ref id e2)
  | Lbreak -> Lambda.break
  | Lcontinue -> Lambda.continue
  | Lwhile (e1, e2) -> Lambda.while_ (eliminate_ref id e1) (eliminate_ref id e2)
  | Lfor (v, e1, e2, dir, e3) ->
    Lambda.for_ v (eliminate_ref id e1) (eliminate_ref id e2) dir
      (eliminate_ref id e3)
  | Lfor_of (v, e1, e2) ->
    Lambda.for_of v (eliminate_ref id e1) (eliminate_ref id e2)
  | Lfor_await_of (v, e1, e2) ->
    Lambda.for_await_of v (eliminate_ref id e1) (eliminate_ref id e2)
  | Lassign (v, e) -> Lambda.assign v (eliminate_ref id e)
