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

(*A naive dead code elimination *)
type used_info = {
  mutable times: int;
  mutable captured: bool;
      (* captured in functon or loop,
         inline in such cases should be careful
         1. can not inline mutable values
         2. avoid re-computation
      *)
}

type occ_tbl = used_info Hash_ident.t
(* First pass: count the occurrences of all let-bound identifiers *)

type local_tbl = used_info Map_ident.t

let dummy_info () = {times = 0; captured = false}
(* y is untouched *)

let absorb_info (x : used_info) (y : used_info) =
  match (x, y) with
  | {times = x0}, {times = y0; captured} ->
    x.times <- x0 + y0;
    if captured then x.captured <- true

let pp_info fmt (x : used_info) =
  Format.fprintf fmt "(<captured:%b>:%d)" x.captured x.times

let pp_occ_tbl fmt tbl =
  Hash_ident.iter tbl (fun k v ->
      Format.fprintf fmt "@[%a@ %a@]@." Ident.print k pp_info v)

(* The global table [occ] associates to each let-bound identifier
   the number of its uses (as a reference):
   - 0 if never used
   - 1 if used exactly once in and not under a lambda or within a loop
   - when under a lambda,
   - it's probably a closure
   - within a loop
   - update reference,
       niether is good for inlining
   - > 1 if used several times or under a lambda or within a loop.
     The local table [bv] associates to each locally-let-bound variable
     its reference count, as above.  [bv] is enriched at let bindings
     but emptied when crossing lambdas and loops. *)
let collect_occurs lam : occ_tbl =
  let occ : occ_tbl = Hash_ident.create 83 in

  (* Current use count of a variable. *)
  let used v =
    match Hash_ident.find_opt occ v with
    | None -> false
    | Some {times; _} -> times > 0
  in

  (* Entering a [let].  Returns updated [bv]. *)
  let bind_var bv ident =
    let r = dummy_info () in
    Hash_ident.add occ ident r;
    Map_ident.add bv ident r
  in

  (* Record a use of a variable *)
  let add_one_use bv ident =
    match Map_ident.find_opt bv ident with
    | Some r -> r.times <- r.times + 1
    | None -> (
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Hash_ident.find_opt occ ident with
      | Some r -> absorb_info r {times = 1; captured = true}
      | None ->
        (* Not a let-bound variable, ignore *)
        ())
  in

  let inherit_use bv ident bid =
    let n =
      match Hash_ident.find_opt occ bid with
      | None -> dummy_info ()
      | Some v -> v
    in
    match Map_ident.find_opt bv ident with
    | Some r -> absorb_info r n
    | None -> (
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Hash_ident.find_opt occ ident with
      | Some r -> absorb_info r {n with captured = true}
      | None ->
        (* Not a let-bound variable, ignore *)
        ())
  in

  let rec count (bv : local_tbl) (lam : Lam.t) =
    match lam with
    | Lfunction {body = l} -> count Map_ident.empty l
    (* when entering a function local [bv]
        is cleaned up, so that all closure variables will not be
        carried over, since the parameters are never rebound,
        so it is fine to kep it empty
    *)
    | Lfor (_, l1, l2, _dir, l3) ->
      count bv l1;
      count bv l2;
      count Map_ident.empty l3
    | Lwhile (l1, l2) ->
      count Map_ident.empty l1;
      count Map_ident.empty l2
    | Lvar v -> add_one_use bv v
    | Llet (_, v, Lvar w, l2) ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      inherit_use bv w v
    | Llet (kind, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* count [l2] first,
         If v is unused, l1 will be removed, so don't count its variables *)
      if kind = Strict || used v then count bv l1
    | Lassign (_, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         this ident's refcount *)
      count bv l
    | Lglobal_module _ -> ()
    | Lprim {args; _} -> List.iter (count bv) args
    | Lletrec (bindings, body) ->
      List.iter (fun (_v, l) -> count bv l) bindings;
      count bv body
    (* Note there is a difference here when do beta reduction for *)
    | Lapply
        {ap_func = Lfunction ({params; body} as lfunction); ap_args = args; _}
      when Ext_list.same_length params args
           && Lam_analysis.lfunction_can_be_inlined lfunction ->
      count bv (Lam_beta_reduce.no_names_beta_reduce params body args)
    (* | Lapply{fn = Lfunction{function_kind = Tupled; params; body}; *)
    (*          args = [Lprim {primitive = Pmakeblock _;  args; _}]; _} *)
    (*   when  Ext_list.same_length params  args -> *)
    (*   count bv (Lam_beta_reduce.beta_reduce   params body args) *)
    | Lapply {ap_func = l1; ap_args = ll; _} ->
      count bv l1;
      List.iter (count bv) ll
    | Lconst _cst -> ()
    | Lswitch (l, sw) ->
      count_default bv sw;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
    | Lstringswitch (l, sw, d) -> (
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw;
      match d with
      | Some d -> count bv d
      | None -> ())
    (* x2 for native backend *)
    (* begin match sw with *)
    (* | []|[_] -> count bv d *)
    (* | _ -> count bv d ; count bv d *)
    (* end *)
    | Lstaticraise (_i, ls) -> List.iter (count bv) ls
    | Lstaticcatch (l1, (_i, _), l2) ->
      count bv l1;
      count bv l2
    | Ltrywith (l1, _v, l2) ->
      count bv l1;
      count bv l2
    | Lifthenelse (l1, l2, l3) ->
      count bv l1;
      count bv l2;
      count bv l3
    | Lsequence (l1, l2) ->
      count bv l1;
      count bv l2
  and count_default bv sw =
    match sw.sw_failaction with
    | None -> ()
    | Some al ->
      if (not sw.sw_consts_full) && not sw.sw_blocks_full then (
        (* default action will occur twice in native code *)
        count bv al;
        count bv al)
      else (
        (* default action will occur once *)
        assert ((not sw.sw_consts_full) || not sw.sw_blocks_full);
        count bv al)
  in
  count Map_ident.empty lam;
  occ
