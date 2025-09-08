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
  let f x y =  x + y 
  Invariant: there is no currying 
  here since f's arity is 2, no side effect 
  f 3 --> function(y) -> f 3 y 
*)

(** 
   [transform n loc status fn args]
   n is the number of missing arguments required for [fn].
   Return a function of airty [n]
*)
let transform_under_supply n ap_info fn args =
  let extra_args = Ext_list.init n (fun _ -> Ident.create Literals.param) in
  let extra_lambdas = Ext_list.map extra_args Lam.var in
  match
    Ext_list.fold_right (fn :: args) ([], []) (fun (lam : Lam.t) (acc, bind) ->
        match lam with
        | Lvar _
        | Lconst
            ( Const_int _ | Const_char _ | Const_string _ | Const_float _
            | Const_bigint _ | Const_pointer _ | Const_js_true | Const_js_false
            | Const_js_undefined _ )
        | Lprim {primitive = Pfield (_, Fld_module _); _}
        | Lfunction _ ->
          (lam :: acc, bind)
        | _ ->
          let v = Ident.create Literals.partial_arg in
          (Lam.var v :: acc, (v, lam) :: bind))
  with
  | fn :: args, [] ->
    (* More than no side effect in the [args],
       we try to avoid computation, so even if
       [x + y] is side effect free, we need eval it only once
    *)
    (* TODO: Note we could adjust [fn] if [fn] is already a function
       But it is dangerous to change the arity
       of an existing function which may cause inconsistency
    *)
    Lam.function_ ~arity:n ~params:extra_args
      ~attr:Lambda.default_function_attribute
      ~body:(Lam.apply fn (Ext_list.append args extra_lambdas) ap_info)
  | fn :: args, bindings ->
    let rest : Lam.t =
      Lam.function_ ~arity:n ~params:extra_args
        ~attr:Lambda.default_function_attribute
        ~body:(Lam.apply fn (Ext_list.append args extra_lambdas) ap_info)
    in
    Ext_list.fold_left bindings rest (fun lam (id, x) ->
        Lam.let_ Strict id x lam)
  | _, _ -> assert false

(* Invariant: mk0 : (unit -> 'a0) -> 'a0 t
                TODO: this case should be optimized,
                we need check where we handle [arity=0]
                as a special case --
                if we do an optimization before compiling
                into lambda

   {[Fn.mk0]} is not intended for use by normal users

   so we assume [Fn.mk0] is only used in such cases
   {[
     Fn.mk0 (fun _ -> .. )
   ]}
   when it is passed as a function directly
*)
(*TODO: can be optimized ?
  {[\ x y -> (\u -> body x) x y]}
  {[\u x -> body x]}
    rewrite rules
  {[
    \x -> body
          --
          \y (\x -> body ) y
  ]}
  {[\ x y -> (\a b c -> g a b c) x y]}
  {[ \a b -> \c -> g a b c ]}
*)

(** Unsafe function, we are changing arity here, it should be applied 
    cautiously, since 
    [let u = f] and we are chaning the arity of [f] it will affect 
    the collection of [u]
    A typical use case is to pass an OCaml function to JS side as a callback (i.e, [@uncurry])
*)
let unsafe_adjust_to_arity loc ~(to_ : int) ?(from : int option) (fn : Lam.t) :
    Lam.t =
  let ap_info : Lam.ap_info =
    {ap_loc = loc; ap_inlined = Default_inline; ap_status = App_na}
  in
  let is_async_fn =
    match fn with
    | Lfunction {attr = {async}} -> async
    | _ -> false
  in
  match (from, fn) with
  | Some from, _ | None, Lfunction {arity = from} -> (
    if from = to_ || is_async_fn then fn
    else if to_ = 0 then
      match fn with
      | Lfunction {params = [param]; body} ->
        Lam.function_ ~arity:0 ~attr:Lambda.default_function_attribute
          ~params:[]
          ~body:(Lam.let_ Alias param Lam.unit body)
        (* could be only introduced by
           {[ Pjs_fn_make 0 ]} <-
           {[ fun [@bs] () -> .. ]}
        *)
      | _ -> (
        let wrapper, new_fn =
          match fn with
          | Lvar _
          | Lprim
              {
                primitive = Pfield (_, Fld_module _);
                args = [(Lglobal_module _ | Lvar _)];
                _;
              } ->
            (None, fn)
          | _ ->
            let partial_arg = Ext_ident.create Literals.partial_arg in
            (Some partial_arg, Lam.var partial_arg)
        in

        let cont =
          Lam.function_ ~attr:Lambda.default_function_attribute ~arity:0
            ~params:[]
            ~body:(Lam.apply new_fn [Lam.unit] ap_info)
        in

        match wrapper with
        | None -> cont
        | Some partial_arg -> Lam.let_ Strict partial_arg fn cont)
    else if to_ > from then
      match fn with
      | Lfunction {params; body} ->
        (* {[fun x -> f]} ->
           {[ fun x y -> f y ]}
        *)
        let extra_args =
          Ext_list.init (to_ - from) (fun _ -> Ident.create Literals.param)
        in
        let rec mk_apply body vars =
          match vars with
          | [] -> body
          | var :: vars -> mk_apply (Lam.apply body [var] ap_info) vars
        in
        Lam.function_ ~attr:Lambda.default_function_attribute ~arity:to_
          ~params:(Ext_list.append params extra_args)
          ~body:(mk_apply body (Ext_list.map extra_args Lam.var))
      | _ -> (
        let arity = to_ in
        let extra_args =
          Ext_list.init to_ (fun _ -> Ident.create Literals.param)
        in
        let wrapper, new_fn =
          match fn with
          | Lvar _
          | Lprim
              {
                primitive = Pfield (_, Fld_module _);
                args = [(Lglobal_module _ | Lvar _)];
                _;
              } ->
            (None, fn)
          | _ ->
            let partial_arg = Ext_ident.create Literals.partial_arg in
            (Some partial_arg, Lam.var partial_arg)
        in
        let cont =
          Lam.function_ ~arity ~attr:Lambda.default_function_attribute
            ~params:extra_args
            ~body:
              (let first_args, rest_args = Ext_list.split_at extra_args from in
               Lam.apply
                 (Lam.apply new_fn
                    (Ext_list.map first_args Lam.var)
                    {ap_info with ap_status = App_infer_full})
                 (Ext_list.map rest_args Lam.var)
                 ap_info)
        in
        match wrapper with
        | None -> cont
        | Some partial_arg -> Lam.let_ Strict partial_arg fn cont)
    else
      (* add3  --adjust to arity 1 ->
         fun x -> (fun y z -> add3 x y z )

         [fun x y z -> f x y z ]
         [fun x -> [fun y z -> f x y z ]]
         This is okay if the function is not held by other..
      *)
      match fn with
      | Lfunction {params; body}
      (* TODO check arity = List.length params in debug mode *) ->
        let arity = to_ in
        let extra_outer_args, extra_inner_args =
          Ext_list.split_at params arity
        in
        Lam.function_ ~arity ~attr:Lambda.default_function_attribute
          ~params:extra_outer_args
          ~body:
            (Lam.function_ ~arity:(from - to_)
               ~attr:Lambda.default_function_attribute ~params:extra_inner_args
               ~body)
      | _ -> (
        let extra_outer_args =
          Ext_list.init to_ (fun _ -> Ident.create Literals.param)
        in
        let wrapper, new_fn =
          match fn with
          | Lvar _
          | Lprim
              {
                primitive = Pfield (_, Fld_module _);
                args = [(Lglobal_module _ | Lvar _)];
                _;
              } ->
            (None, fn)
          | _ ->
            let partial_arg = Ext_ident.create Literals.partial_arg in
            (Some partial_arg, Lam.var partial_arg)
        in
        let cont =
          Lam.function_ ~arity:to_ ~params:extra_outer_args
            ~attr:Lambda.default_function_attribute
            ~body:
              (let arity = from - to_ in
               let extra_inner_args =
                 Ext_list.init arity (fun _ -> Ident.create Literals.param)
               in
               Lam.function_ ~arity ~params:extra_inner_args
                 ~attr:Lambda.default_function_attribute
                 ~body:
                   (Lam.apply new_fn
                      (Ext_list.map_append extra_outer_args
                         (Ext_list.map extra_inner_args Lam.var)
                         Lam.var)
                      {ap_info with ap_status = App_infer_full}))
        in
        match wrapper with
        | None -> cont
        | Some partial_arg -> Lam.let_ Strict partial_arg fn cont))
  | None, _ ->
    (* In this case [fn] is not [Lfunction], otherwise we would get [arity] *)
    if to_ = 0 then
      let wrapper, new_fn =
        match fn with
        | Lvar _
        | Lprim
            {
              primitive = Pfield (_, Fld_module _);
              args = [(Lglobal_module _ | Lvar _)];
              _;
            } ->
          (None, fn)
        | _ ->
          let partial_arg = Ext_ident.create Literals.partial_arg in
          (Some partial_arg, Lam.var partial_arg)
      in

      let cont =
        Lam.function_ ~attr:Lambda.default_function_attribute ~arity:0
          ~params:[]
          ~body:(Lam.apply new_fn [Lam.unit] ap_info)
      in

      match wrapper with
      | None -> cont
      | Some partial_arg -> Lam.let_ Strict partial_arg fn cont
    else transform_under_supply to_ ap_info fn []

(* | _ ->
   let partial_arg = Ext_ident.create Literals.partial_arg in
   Lam.let_ Strict partial_arg fn
    (let arity = to_ in
     let extra_args = Ext_list.init arity (fun _ -> Ident.create Literals.param) in
     Lam.function_ ~arity ~kind:Curried ~params:extra_args
       ~body:(Lam.apply fn (Ext_list.map Lam.var extra_args ) loc Lam.App_na )
    ) *)
