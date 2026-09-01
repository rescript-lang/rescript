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
            ( Const_int _ | Const_assertfalse | Const_constructor _
            | Const_char _ | Const_string _ | Const_float _ | Const_bigint _
            | Const_polyvar _ | Const_js_true | Const_js_false
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
    Lam.function_ ~loc:Location.none ~arity:n ~params:extra_args
      ~attr:Lambda.default_function_attribute
      ~body:(Lam.apply fn (Ext_list.append args extra_lambdas) ap_info)
  | fn :: args, bindings ->
    let rest : Lam.t =
      Lam.function_ ~loc:Location.none ~arity:n ~params:extra_args
        ~attr:Lambda.default_function_attribute
        ~body:(Lam.apply fn (Ext_list.append args extra_lambdas) ap_info)
    in
    Ext_list.fold_left bindings rest (fun lam (id, x) ->
        Lam.let_ Strict id x lam)
  | _, _ -> assert false
