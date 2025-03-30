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

     Base line
    {[
      for i = 1 to n do (function (i){...}(i))
      done
      (* This is okay, since all ocaml follow the lexical scope,
         for generrated code too (* TODO: check *)
      *)
    ]}

    For nested loops
    {[
      for i = 0 to n do
        for j = 0 to n do
          arrr.(j)<- ()=>{ i}
        done
      done
    ]}
     Three kind of variables  (defined in the loop scope)
     1. loop mutable variables
        As long as variables change per iteration, defined in a loop (in the same loop)
         and captured by a closure
        the loop, iff  be lexically scoped
        Tailcall parameters are considered defined inside the loop
    - unless it's defined
        outside all the loops - note that for nested loops, if it's defined
        in the outerloop and captured by the inner loop,
        it still has to be lexically scoped.

        How do we detect whether it is loop invariant or not
    - depend on loop variant
    - depend on mutuable valuse
    - non pure (function call)

        so we need collect mutable variables
        1. from lambda + loop (for/i) + tailcall params
        2. defined in the loop and can not determine it is invariant
           in such cases we can determine it's immutable
           1. const
           2. only depend on immutable values and no function call?

     ## The following would take advantage of nested loops
     2. loop invariant observable varaibles
    {[
      var x = (console.log(3), 32)
    ]}
     3. loop invariant non-observable variables

     Invariant:
     loop invariant (observable or not) variables can not depend on
     loop mutable values so that once we detect loop Invariant variables
     all its dependency are loop invariant as well, so we can do loop
     Invariant code motion.

     TODO:
     loop invariant can be layered, it will be loop invariant
     in the inner layer while loop variant in the outer layer.
    {[
      for i = 0 to 10 do
        for j  = 10 do
          let  k0 = param * 100 in (* loop invariant *)
          let  k1 = i * i in (* inner loop invariant, loop variant *)
          let  k2 = j * i in (* variant *)
          ..
        done
      done
    ]}
*)
type state = {
  defined_idents: Set_ident.t;
  used_idents: Set_ident.t;
  loop_mutable_values: Set_ident.t;
  mutable_values: Set_ident.t;
  closured_idents: Set_ident.t;
  in_loop: bool;
}

let init_state =
  {
    defined_idents = Set_ident.empty;
    used_idents = Set_ident.empty;
    loop_mutable_values = Set_ident.empty;
    mutable_values = Set_ident.empty;
    closured_idents = Set_ident.empty;
    in_loop = false;
  }

let with_in_loop (st : state) b =
  if b = st.in_loop then st else {st with in_loop = b}

let add_loop_mutable_variable (st : state) id =
  {
    st with
    loop_mutable_values = Set_ident.add st.loop_mutable_values id;
    mutable_values = Set_ident.add st.mutable_values id;
  }

let add_mutable_variable (st : state) id =
  {st with mutable_values = Set_ident.add st.mutable_values id}

let add_defined_ident (st : state) id =
  {st with defined_idents = Set_ident.add st.defined_idents id}

let add_used_ident (st : state) id =
  {st with used_idents = Set_ident.add st.used_idents id}

let super = Js_record_fold.super

let record_scope_pass =
  {
    super with
    expression =
      (fun self state x ->
        match x.expression_desc with
        | Fun {params; body; env} ->
          (* Function is the only place to introduce a new scope in
              ES5
              TODO: check
             {[ try .. catch(exn) {.. }]}
              what's the scope of exn
          *)
          (* Note that [used_idents] is not complete
              it ignores some locally defined idents *)
          let param_set = Set_ident.of_list params in
          let {defined_idents = defined_idents'; used_idents = used_idents'} =
            self.block self
              {
                init_state with
                mutable_values =
                  Set_ident.of_list (Js_fun_env.get_mutable_params params env);
              }
              body
          in
          (* let defined_idents', used_idents' =
             obj#get_defined_idents, obj#get_used_idents  in *)
          (* mark which param is used *)
          params
          |> List.iteri (fun i v ->
                 if not (Set_ident.mem used_idents' v) then
                   Js_fun_env.mark_unused env i);
          let closured_idents' =
            (* pass param_set down *)
            Set_ident.(diff used_idents' (union defined_idents' param_set))
          in

          (* Noe that we don't know which variables are exactly mutable yet ..
             due to the recursive thing
          *)
          Js_fun_env.set_unbounded env closured_idents';
          (* tailcall , note that these varibles are used in another pass *)
          {
            state with
            used_idents = Set_ident.union state.used_idents closured_idents';
            (* There is a bug in ocaml -dsource*)
            closured_idents =
              Set_ident.union state.closured_idents closured_idents';
          }
        | _ -> (
          let obj = super.expression self state x in
          match Js_block_runtime.check_additional_id x with
          | None -> obj
          | Some id -> add_used_ident obj id));
    variable_declaration =
      (fun self state x ->
        match x with
        | {ident; value; property} -> (
          let obj =
            add_defined_ident
              (match (state.in_loop, property) with
              | true, Variable -> add_loop_mutable_variable state ident
              | true, (Strict | StrictOpt | Alias)
              (* Not real true immutable in javascript
                  since it's in the loop

                  TODO: we should also
              *)
                -> (
                match value with
                | None ->
                  add_loop_mutable_variable state ident
                  (* TODO: Check why assertion failure *)
                  (* self#add_loop_mutable_variable ident *)
                  (* assert false *)
                | Some x -> (
                  (*
                      when x is an immediate immutable value, 
                      (like integer .. )
                      not a reference, it should be Immutable
                      or string, 
                      type system might help here
                      TODO:
                  *)
                  match x.expression_desc with
                  | Fun _ | Number _ | Str _ -> state
                  | _ ->
                    (* if Set_ident.(is_empty @@ *)
                    (*   inter self#get_mutable_values  *)
                    (*     ( ({<  *)
                    (*         defined_idents = Set_ident.empty;  *)
                    (*         used_idents = Set_ident.empty; *)
                    (*         >} # expression x) # get_used_idents)) then *)
                    (*   (\* FIXME: still need to check expression is pure or not*\) *)
                    (*   self *)
                    (* else  *)
                    add_loop_mutable_variable state ident))
              | false, Variable -> add_mutable_variable state ident
              | false, (Strict | StrictOpt | Alias) -> state)
              ident
          in
          match value with
          | None -> obj
          | Some x -> self.expression self obj x));
    statement =
      (fun self state x ->
        match x.statement_desc with
        | ForRange (_, _, loop_id, _, _) ->
          (* TODO: simplify definition of For *)
          let {
            defined_idents = defined_idents';
            used_idents = used_idents';
            closured_idents = closured_idents';
          } =
            super.statement self
              {
                in_loop = true;
                loop_mutable_values = Set_ident.singleton loop_id;
                used_idents = Set_ident.empty;
                (* TODO: if unused, can we generate better code? *)
                defined_idents = Set_ident.singleton loop_id;
                closured_idents = Set_ident.empty;
                (* Think about nested for blocks *)
                (* Invariant: Finish id is never used *)
                mutable_values = state.mutable_values;
              }
              x
          in

          (* CHECK*)

          (* let defined_idents', used_idents', closured_idents' =
             obj#get_defined_idents, obj#get_used_idents, obj#get_closured_idents in *)
          let lexical_scope =
            Set_ident.(
              inter
                (diff closured_idents' defined_idents')
                state.loop_mutable_values)
          in
          {
            state with
            used_idents = Set_ident.union state.used_idents used_idents';
            (* walk around ocaml -dsource bug
               {[
                 Set_ident.(union used_idents used_idents)
               ]}
            *)
            defined_idents =
              Set_ident.union state.defined_idents defined_idents';
            (* TODO: if we our generated code also follow lexical scope,
               this is not necessary ;
               [varaibles] are mutable or not is known at definition
            *)
            closured_idents =
              Set_ident.union state.closured_idents lexical_scope;
          }
        | While (pred, body) ->
          with_in_loop
            (self.block self
               (with_in_loop (self.expression self state pred) true)
               body)
            state.in_loop
        | _ -> super.statement self state x);
    exception_ident =
      (fun _ state x ->
        (* we can not simply skip it, since it can be used
            TODO: check loop exception
            (loop {
            excption(i){
            () => {i}
            }
            })
        *)
        {
          state with
          used_idents = Set_ident.add state.used_idents x;
          defined_idents = Set_ident.add state.defined_idents x;
        });
    for_ident =
      (fun _ state x ->
        {
          state with
          loop_mutable_values = Set_ident.add state.loop_mutable_values x;
        });
    ident =
      (fun _ state x ->
        if Set_ident.mem state.defined_idents x then state
        else {state with used_idents = Set_ident.add state.used_idents x});
  }

let program js =
  (record_scope_pass.program record_scope_pass init_state js)
    .loop_mutable_values
(* (scope_pass # program js ) # get_loop_mutable_values *)
