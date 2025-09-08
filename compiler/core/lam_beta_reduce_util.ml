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

(*
    Principle: since in ocaml, the apply order is not specified
    rules:
    1. each argument it is only used once, (avoid eval duplication)
    2. it's actually used, if not (Lsequence)
    3. no nested  compuation,
       other wise the evaluation order is tricky (make sure eval order is correct)
*)

type value = {mutable used: bool; lambda: Lam.t}

let param_hash : _ Hash_ident.t = Hash_ident.create 20

(* optimize cases like
   (fun f (a,b){ g (a,b,1)} (e0, e1))
   cases like
   (fun f (a,b){ g (a,b,a)} (e0, e1)) needs avoids double eval

    Note in a very special case we can avoid any allocation
   {[
     when Ext_list.for_all2_no_exn
         (fun p a ->
            match (a : Lam.t) with
            | Lvar a -> Ident.same p a
            | _ -> false ) params args'
   ]}
*)
let simple_beta_reduce params body args =
  let exception Not_simple_apply in
  let find_param_exn v opt =
    match Hash_ident.find_opt param_hash v with
    | Some exp ->
      if exp.used then raise_notrace Not_simple_apply else exp.used <- true;
      exp.lambda
    | None -> opt
  in
  let rec aux_exn acc (us : Lam.t list) =
    match us with
    | [] -> List.rev acc
    | (Lvar x as a) :: rest -> aux_exn (find_param_exn x a :: acc) rest
    | (Lconst _ as u) :: rest -> aux_exn (u :: acc) rest
    | _ :: _ -> raise_notrace Not_simple_apply
  in
  match (body : Lam.t) with
  | Lprim {primitive; args = ap_args; loc = ap_loc}
  (* There is no lambda in primitive *) -> (
    (* catch a special case of primitives *)
    let () =
      List.iter2
        (fun p a -> Hash_ident.add param_hash p {lambda = a; used = false})
        params args
    in
    try
      let new_args = aux_exn [] ap_args in
      let result =
        Hash_ident.fold param_hash (Lam.prim ~primitive ~args:new_args ap_loc)
          (fun _param stats acc ->
            let {lambda; used} = stats in
            if not used then Lam.seq lambda acc else acc)
      in
      Hash_ident.clear param_hash;
      Some result
    with Not_simple_apply ->
      Hash_ident.clear param_hash;
      None)
  | Lapply
      {
        ap_func =
          (Lvar _ | Lprim {primitive = Pfield _; args = [Lglobal_module _]}) as
          f;
        ap_args;
        ap_info;
      } -> (
    let () =
      List.iter2
        (fun p a -> Hash_ident.add param_hash p {lambda = a; used = false})
        params args
    in
    (*since we adde each param only once,
      iff it is removed once, no exception,
      if it is removed twice there will be exception.
      if it is never removed, we have it as rest keys
    *)
    try
      let new_args = aux_exn [] ap_args in
      let f =
        match f with
        | Lvar fn_name -> find_param_exn fn_name f
        | _ -> f
      in
      let result =
        Hash_ident.fold param_hash (Lam.apply f new_args ap_info)
          (fun _param stat acc ->
            let {lambda; used} = stat in
            if not used then Lam.seq lambda acc else acc)
      in
      Hash_ident.clear param_hash;
      Some result
    with Not_simple_apply ->
      Hash_ident.clear param_hash;
      None)
  | _ -> None
