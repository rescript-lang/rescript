(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

module E = Js_exp_make
module S = Js_stmt_make

type meta_info = Info of J.ident_info | Recursive

let super = Js_record_iter.super

let mark_dead_code (js : J.program) : J.program =
  let ident_use_stats : meta_info Hash_ident.t = Hash_ident.create 17 in
  let mark_dead =
    {
      super with
      ident =
        (fun _ ident ->
          match Hash_ident.find_opt ident_use_stats ident with
          | None ->
            (* First time *)
            Hash_ident.add ident_use_stats ident Recursive
          (* recursive identifiers *)
          | Some Recursive -> ()
          | Some (Info x) -> Js_op_util.update_used_stats x Used);
      variable_declaration =
        (fun self vd ->
          match vd.ident_info.used_stats with
          | Dead_pure -> ()
          | Dead_non_pure -> (
            match vd.value with
            | None -> ()
            | Some x -> self.expression self x)
          | _ -> (
            let ({ident; ident_info; value; _} : J.variable_declaration) = vd in
            let pure =
              match value with
              | None -> true
              | Some x ->
                self.expression self x;
                Js_analyzer.no_side_effect_expression x
            in
            let () =
              if Set_ident.mem js.export_set ident then
                Js_op_util.update_used_stats ident_info Exported
            in
            match Hash_ident.find_opt ident_use_stats ident with
            | Some Recursive ->
              Js_op_util.update_used_stats ident_info Used;
              Hash_ident.replace ident_use_stats ident (Info ident_info)
            | Some (Info _) ->
              (* check [camlinternlFormat,box_type] inlined twice
                  FIXME: seems we have redeclared identifiers
              *)
              ()
            (* assert false *)
            | None ->
              (* First time *)
              Hash_ident.add ident_use_stats ident (Info ident_info);
              Js_op_util.update_used_stats ident_info
                (if pure then Scanning_pure else Scanning_non_pure)));
    }
  in
  mark_dead.program mark_dead js;
  Hash_ident.iter ident_use_stats (fun _id (info : meta_info) ->
      match info with
      | Info ({used_stats = Scanning_pure} as info) ->
        Js_op_util.update_used_stats info Dead_pure
      | Info ({used_stats = Scanning_non_pure} as info) ->
        Js_op_util.update_used_stats info Dead_non_pure
      | _ -> ());
  js

(*
   when we do optmizations, we might need track it will break invariant 
   of other optimizations, especially for [mutable] meta data, 
   for example, this pass will break [closure] information, 
   it should be done before closure pass (even it does not use closure information)

   Take away, it is really hard to change the code while collecting some information..
   we should always collect info in a single pass

   Note that, we should avoid reuse object, i.e,
   {[
     let v = 
       object 
       end      
   ]}   
   Since user may use `bsc.exe -c xx.ml xy.ml xz.ml` and we need clean up state
*)

(** we can do here, however, we should 
    be careful that it can only be done 
    when it's accessed once and the array is not escaped,
    otherwise, we redo the computation,
    or even better, we re-order

    {[
      var match = [/* tuple */0,Pervasives.string_of_int(f(1,2,3)),f3(2),arr];

          var a = match[1];

            var b = match[2];

    ]}

    --->

    {[
      var match$1 = Pervasives.string_of_int(f(1,2,3));
          var match$2 = f3(2);
              var match = [/* tuple */0,match$1,match$2,arr];
                  var a = match$1;
                    var b = match$2;
                      var arr = arr; 
    ]}

    --> 
    since match$1 (after match is eliminated) is only called once 
    {[
      var a = Pervasives.string_of_int(f(1,2,3));
      var b = f3(2);
      var arr = arr; 
    ]}

*)

let super = Js_record_map.super

let add_substitue substitution (ident : Ident.t) (e : J.expression) =
  Hash_ident.replace substitution ident e

let subst_map (substitution : J.expression Hash_ident.t) =
  {
    super with
    statement =
      (fun self v ->
        match v.statement_desc with
        | Variable {ident = _; ident_info = {used_stats = Dead_pure}; _} ->
          {v with statement_desc = Block []}
        | Variable
            {ident = _; ident_info = {used_stats = Dead_non_pure}; value = None}
          ->
          {v with statement_desc = Block []}
        | Variable
            {
              ident = _;
              ident_info = {used_stats = Dead_non_pure};
              value = Some x;
            } ->
          {v with statement_desc = Exp x}
        | Variable
            ({
               ident;
               property = Strict | StrictOpt | Alias;
               value =
                 Some
                   ({
                      expression_desc =
                        Caml_block
                          ((_ :: _ :: _ as ls), Immutable, tag, tag_info);
                    } as block);
             } as variable) -> (
          (* If we do this, we should prevent incorrect inlning to inline it into an array :)
              do it only when block size is larger than one
          *)
          let _, e, bindings =
            Ext_list.fold_left ls (0, [], []) (fun (i, e, acc) x ->
                match x.expression_desc with
                | Var _ | Number _ | Str _ | J.Bool _ | Undefined _ ->
                  (* TODO: check the optimization *)
                  (i + 1, x :: e, acc)
                | _ ->
                  (* tradeoff,
                      when the block is small, it does not make
                      sense too much --
                      bottomline, when the block size is one, no need to do
                      this
                  *)
                  let v' = self.expression self x in
                  let match_id =
                    Ext_ident.create
                      (ident.name ^ "_"
                      ^
                      match tag_info with
                      | Blk_module fields -> (
                        match Ext_list.nth_opt fields i with
                        | None -> Printf.sprintf "%d" i
                        | Some x -> x)
                      | Blk_record {fields} -> (
                        try fst fields.(i) with _ -> Printf.sprintf "%d" i)
                      | _ -> Printf.sprintf "%d" i)
                  in
                  (i + 1, E.var match_id :: e, (match_id, v') :: acc))
          in
          let e =
            {
              block with
              expression_desc = Caml_block (List.rev e, Immutable, tag, tag_info);
            }
          in
          let () = add_substitue substitution ident e in
          (* let bindings =  !bindings in *)
          let original_statement =
            {v with statement_desc = Variable {variable with value = Some e}}
          in
          match bindings with
          | [] -> original_statement
          | _ ->
            (* self#add_substitue ident e ; *)
            S.block
            @@ Ext_list.rev_map_append bindings [original_statement]
                 (fun (id, v) -> S.define_variable ~kind:Strict id v))
        | _ -> super.statement self v);
    expression =
      (fun self x ->
        match x.expression_desc with
        | Array_index
            ( {expression_desc = Var (Id id)},
              {expression_desc = Number (Int {i; _})} )
        | Static_index ({expression_desc = Var (Id id)}, _, Some i) -> (
          match Hash_ident.find_opt substitution id with
          | Some {expression_desc = Caml_block (ls, Immutable, _, _)} -> (
            (* user program can be wrong, we should not
               turn a runtime crash into compile time crash : )
            *)
            match Ext_list.nth_opt ls (Int32.to_int i) with
            (* 7432: prevent optimization in JSX preserve mode *)
            | Some {expression_desc = J.Var (Id {name = "make"})}
              when !Js_config.jsx_preserve ->
              super.expression self x
            | Some
                ({expression_desc = J.Var _ | Number _ | Str _ | Undefined _} as
                 x) ->
              x
            | None | Some _ -> super.expression self x)
          | Some _ | None -> super.expression self x)
        | _ -> super.expression self x);
  }

(* Top down or bottom up ?*)
(* A pass to support nullary argument in JS
    Nullary information can be done in one pass,
    there is no need to add another pass
*)

let program (js : J.program) =
  let obj = subst_map (Hash_ident.create 32) in
  let js = obj.program obj js in
  mark_dead_code js
(* |> mark_dead_code *)
(* mark dead code twice does have effect in some cases, however, we disabled it
   since the benefit is not obvious
*)
