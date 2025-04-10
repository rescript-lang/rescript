(* Copyright (C) 2015- Hongbo Zhang, Authors of ReScript
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

(* type eliminate =
   |  Not_eliminatable
   | *)

let rec eliminate_tuple (id : Ident.t) (lam : Lam.t) acc =
  match lam with
  | Llet (Alias, v, Lprim {primitive = Pfield (i, _); args = [Lvar tuple]}, e2)
    when Ident.same tuple id ->
    eliminate_tuple id e2 (Map_int.add acc i v)
  (* it is okay to have duplicates*)
  | _ -> if Lam_hit.hit_variable id lam then None else Some (acc, lam)
(* [groups] are in reverse order *)

(* be careful to flatten letrec
    like below :
    {[
      let rec even =
        let odd n =  if n ==1 then true else even (n - 1) in
        fun n -> if n ==0  then true else odd (n - 1)
    ]}
    odd and even are recursive values, since all definitions inside
    e.g, [odd] can see [even] now, however, it should be fine
    in our case? since ocaml's recursive value does not allow immediate
    access its value direclty?, seems no
    {[
      let rec even2 =
        let odd = even2 in
        fun n -> if n ==0  then true else odd (n - 1)
    ]}
*)
(* FIXME:
    here we try to move inner definitions of [recurisve value] upwards
    for example:
   {[
     let rec x =
       let y = 32 in
       y :: x
     and z = ..
       ---
       le ty = 32 in
     let rec x = y::x
     and z = ..
   ]}
    however, the inner definitions can see [z] and [x], so we
    can not blindly move it in the beginning, however, for
    recursive value, ocaml does not allow immediate access to
    recursive value, so what's the best strategy?
    ---
    the motivation is to capture real tail call
*)
(* | Single ((Alias | Strict | StrictOpt), id, ( Lfunction _ )) ->
   (** FIXME:
   It should be alias and alias will be optimized away
   in later optmizations, however,
   this means if we don't optimize
   {[ let u/a = v in ..]}
                          the output would be wrong, we should *optimize
                          this away right now* instead of delaying it to the
                          later passes
                        *)
                          (acc, set, g :: wrap, stop)
*)
(* could also be from nested [let rec]
          like
   {[
     let rec x =
       let rec y = 1 :: y in
       2:: List.hd y:: x
   ]}
          TODO: seems like we should update depenency graph,
*)

(* Printlambda.lambda Format.err_formatter lam ; assert false  *)

(** TODO: more flattening,
                    - also for function compilation, flattening should be done first
                    - [compile_group] and [compile] become mutually recursive function
                *)
let lambda_of_groups ~(rev_bindings : Lam_group.t list) (result : Lam.t) : Lam.t
    =
  Ext_list.fold_left rev_bindings result (fun acc x ->
      match x with
      | Nop l -> Lam.seq l acc
      | Single (kind, ident, lam) -> Lam_util.refine_let ~kind ident lam acc
      | Recursive bindings -> Lam.letrec bindings acc)

(* TODO:
    refine effectful [ket_kind] to be pure or not
    Be careful of how [Lifused(v,l)] work
    since its semantics depend on whether v is used or not
    return value are in reverse order, but handled by [lambda_of_groups]
*)
let deep_flatten (lam : Lam.t) : Lam.t =
  let rec flatten (acc : Lam_group.t list) (lam : Lam.t) :
      Lam.t * Lam_group.t list =
    match lam with
    | Llet
        ( str,
          id,
          (Lprim
             {
               primitive =
                 Pnull_to_opt | Pundefined_to_opt | Pnull_undefined_to_opt;
               args = [Lvar _];
             } as arg),
          body ) ->
      flatten (Single (str, id, aux arg) :: acc) body
    | Llet
        ( str,
          id,
          Lprim
            {
              primitive =
                (Pnull_to_opt | Pundefined_to_opt | Pnull_undefined_to_opt) as
                primitive;
              args = [arg];
            },
          body ) ->
      let new_id = Ident.rename id in
      flatten acc
        (Lam.let_ str new_id arg
           (Lam.let_ Alias id
              (Lam.prim ~primitive
                 ~args:[Lam.var new_id]
                 Location.none (* FIXME*))
              body))
    | Llet (str, id, arg, body) -> (
      (*
                         {[ let match = (a,b,c)
                           let d = (match/1)
                           let e = (match/2)
                                   ..
                         ]}
                      *)
      let res, accux = flatten acc arg in
      match (id.name, str, res) with
      | ( ("match" | "include" | "param"),
          (Alias | Strict | StrictOpt),
          Lprim {primitive = Pmakeblock (_, _, Immutable); args} ) -> (
        match eliminate_tuple id body Map_int.empty with
        | Some (tuple_mapping, body) ->
          flatten
            (Ext_list.fold_left_with_offset args accux 0 (fun arg acc i ->
                 match Map_int.find_opt tuple_mapping i with
                 | None -> Lam_group.nop_cons arg acc
                 | Some key -> Lam_group.single str key arg :: acc))
            body
        | None -> flatten (Single (str, id, res) :: accux) body)
      | _ -> flatten (Single (str, id, res) :: accux) body)
    | Lletrec (bind_args, body) ->
      flatten (Recursive (Ext_list.map_snd bind_args aux) :: acc) body
    | Lsequence (l, r) ->
      let res, l = flatten acc l in
      flatten (Lam_group.nop_cons res l) r
    | x -> (aux x, acc)
  and aux (lam : Lam.t) : Lam.t =
    match lam with
    | Llet _ ->
      let res, groups = flatten [] lam in
      lambda_of_groups res ~rev_bindings:groups
    | Lletrec (bind_args, body) ->
      (* Attention: don't mess up with internal {let rec} *)
      let rec iter bind_args groups set =
        match bind_args with
        | [] -> (List.rev groups, set)
        | (id, arg) :: rest ->
          iter rest ((id, aux arg) :: groups) (Set_ident.add set id)
      in
      let groups, collections = iter bind_args [] Set_ident.empty in
      (* Try to extract some value definitions from recursive values as [wrap],
         it will stop whenever it find it could not move forward
         {[
           let rec x =
             let y = 1 in
             let z = 2 in
             ...
         ]}
      *)
      let rev_bindings, rev_wrap, _ =
        Ext_list.fold_left groups ([], [], false)
          (fun (inner_recursive_bindings, wrap, stop) (id, lam) ->
            if stop || Lam_hit.hit_variables collections lam then
              ((id, lam) :: inner_recursive_bindings, wrap, true)
            else
              ( inner_recursive_bindings,
                Lam_group.Single (Strict, id, lam) :: wrap,
                false ))
      in
      lambda_of_groups
        ~rev_bindings:rev_wrap (* These bindings are extracted from [letrec] *)
        (Lam.letrec (List.rev rev_bindings) (aux body))
    | Lsequence (l, r) -> Lam.seq (aux l) (aux r)
    | Lconst _ -> lam
    | Lvar _ -> lam
    (* | Lapply(Lfunction(Curried, params, body), args, _) *)
    (*   when  List.length params = List.length args -> *)
    (*     aux (beta_reduce  params body args) *)
    (* | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _) *)
    (*     (\** TODO: keep track of this parameter in ocaml trunk, *)
    (*           can we switch to the tupled backend? *\) *)
    (*   when  List.length params = List.length args -> *)
    (*       aux (beta_reduce params body args) *)
    | Lapply {ap_func = l1; ap_args = ll; ap_info; ap_transformed_jsx} ->
      Lam.apply (aux l1) (Ext_list.map ll aux) ap_info ~ap_transformed_jsx
    (* This kind of simple optimizations should be done each time
       and as early as possible *)
    | Lglobal_module _ -> lam
    | Lprim {primitive; args; loc} ->
      let args = Ext_list.map args aux in
      Lam.prim ~primitive ~args loc
    | Lfunction {arity; params; body; attr} ->
      Lam.function_ ~arity ~params ~body:(aux body) ~attr
    | Lswitch
        ( l,
          {
            sw_failaction;
            sw_consts;
            sw_blocks;
            sw_blocks_full;
            sw_consts_full;
            sw_names;
          } ) ->
      Lam.switch (aux l)
        {
          sw_consts = Ext_list.map_snd sw_consts aux;
          sw_blocks = Ext_list.map_snd sw_blocks aux;
          sw_consts_full;
          sw_blocks_full;
          sw_failaction = Ext_option.map sw_failaction aux;
          sw_names;
        }
    | Lstringswitch (l, sw, d) ->
      Lam.stringswitch (aux l) (Ext_list.map_snd sw aux) (Ext_option.map d aux)
    | Lstaticraise (i, ls) -> Lam.staticraise i (Ext_list.map ls aux)
    | Lstaticcatch (l1, ids, l2) -> Lam.staticcatch (aux l1) ids (aux l2)
    | Ltrywith (l1, v, l2) -> Lam.try_ (aux l1) v (aux l2)
    | Lifthenelse (l1, l2, l3) -> Lam.if_ (aux l1) (aux l2) (aux l3)
    | Lwhile (l1, l2) -> Lam.while_ (aux l1) (aux l2)
    | Lfor (flag, l1, l2, dir, l3) ->
      Lam.for_ flag (aux l1) (aux l2) dir (aux l3)
    | Lassign (v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refaux *)
      Lam.assign v (aux l)
  in
  aux lam
