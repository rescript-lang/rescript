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

(* The fast path of beta reduction, for a body that is a single primitive or
   call on parameters and constants: each argument is substituted for its
   parameter, and an unused argument is sequenced before the body.

   {[ (fun a b -> g b a) e0 e1 ]} becomes {[ g e1 e0 ]}

   The arguments then run in the order the body names the parameters, after
   the unused ones, while the call runs them left to right. The reduction is
   kept only when that changes nothing:
   1. each parameter is used at most once (avoid eval duplication)
   2. two arguments that both have side effects, or where one has side effects
      and the other reads state that they can change, keep their order; the
      variables the body reads count as arguments after the last one
   3. an argument with side effects is not put in an operand that may not be
      evaluated, the right one of [&&] and [||] *)

type value = {
  mutable used: bool;
  lambda: Lambda.t;
  rank: int;
  motion: Lam_analysis.motion;
}

(* Sequence the unused arguments before [body], in parameter order *)
let seq_unused param_hash params body =
  List.fold_right
    (fun param acc ->
      let {lambda; used} = Hash_ident.find param_hash param in
      if not used then Lambda.seq lambda acc else acc)
    params body

(* optimize cases like
   (fun f (a,b){ g (a,b,1)} (e0, e1))
   cases like
   (fun f (a,b){ g (a,b,a)} (e0, e1)) needs avoids double eval

    Note in a very special case we can avoid any allocation
   {[
     when Ext_list.for_all2_no_exn
         (fun p a ->
            match (a : Lambda.t) with
            | Lvar a -> Ident.same p a
            | _ -> false ) params args'
   ]}
*)
let simple_beta_reduce ~assignable params body args =
  (* Substitutions and their [used] bits belong to this reduction: compiler
     requests run concurrently on several domains. *)
  let param_hash : value Hash_ident.t = Hash_ident.create 20 in
  let exception Not_simple_apply in
  (* The rank of a variable the body reads: after every argument *)
  let body_rank = List.length params in
  (* The code the reduced body evaluates, latest first, with its rank in the
     call *)
  let evaluated = ref [] in
  let evaluate ~conditional rank (motion : Lam_analysis.motion) =
    match motion with
    | Effects when conditional -> raise_notrace Not_simple_apply
    | Anywhere | Reads | Effects -> evaluated := (rank, motion) :: !evaluated
  in
  let find_param_exn ~conditional v opt =
    match Hash_ident.find_opt param_hash v with
    | Some exp ->
      if exp.used then raise_notrace Not_simple_apply else exp.used <- true;
      evaluate ~conditional exp.rank exp.motion;
      exp.lambda
    | None ->
      evaluate ~conditional body_rank (Lam_analysis.motion ~assignable opt);
      opt
  in
  (* Substitutes in the operands from left to right. The operands from
     position [conditional_from] on may not be evaluated. *)
  let rec aux_exn ~conditional_from i (us : Lambda.t list) =
    match us with
    | [] -> []
    | u :: rest ->
      let u =
        match u with
        | Lvar x -> find_param_exn ~conditional:(i >= conditional_from) x u
        | Lconst _ -> u
        | _ -> raise_notrace Not_simple_apply
      in
      u :: aux_exn ~conditional_from (i + 1) rest
  in
  (* The reduced term runs the unused arguments first, then what the body
     evaluates *)
  let check_order_exn () =
    let unused =
      Ext_list.filter_map params (fun p ->
          let {used; rank; motion} = Hash_ident.find param_hash p in
          if used then None else Some (rank, motion))
    in
    let rec in_order = function
      | [] -> true
      | (rank, motion) :: later ->
        Ext_list.for_all later (fun (rank', motion') ->
            rank <= rank' || not (Lam_analysis.conflict motion motion'))
        && in_order later
    in
    if not (in_order (unused @ List.rev !evaluated)) then
      raise_notrace Not_simple_apply
  in
  let add_params () =
    List.iteri
      (fun rank (p, a) ->
        Hash_ident.add param_hash p
          {
            lambda = a;
            used = false;
            rank;
            motion = Lam_analysis.motion ~assignable a;
          })
      (List.combine params args)
  in
  match (body : Lambda.t) with
  | Lprim {primitive; args = ap_args; loc = ap_loc}
  (* There is no lambda in primitive *) -> (
    (* catch a special case of primitives *)
    add_params ();
    let conditional_from =
      if Lambda.may_skip_operands primitive then 1 else max_int
    in
    try
      let new_args = aux_exn ~conditional_from 0 ap_args in
      check_order_exn ();
      let result =
        seq_unused param_hash params
          (Lambda.prim ~primitive ~args:new_args ap_loc)
      in
      Some result
    with Not_simple_apply -> None)
  | Lapply
      {
        ap_func =
          (Lvar _ | Lprim {primitive = Pfield _; args = [Lglobal_module _]}) as
          f;
        ap_args;
        ap_loc;
      } -> (
    add_params ();
    (*since we adde each param only once,
      iff it is removed once, no exception,
      if it is removed twice there will be exception.
      if it is never removed, we have it as rest keys
    *)
    try
      (* the function is evaluated before the arguments *)
      let f =
        match f with
        | Lvar fn_name -> find_param_exn ~conditional:false fn_name f
        | _ -> f
      in
      let new_args = aux_exn ~conditional_from:max_int 0 ap_args in
      check_order_exn ();
      let result =
        seq_unused param_hash params (Lambda.apply f new_args ap_loc)
      in
      Some result
    with Not_simple_apply -> None)
  | _ -> None
