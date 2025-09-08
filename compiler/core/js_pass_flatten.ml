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

(* open recursion is hard
   Take cond for example:
   CHECK? Trick semantics difference
   super#statement (S.if_ a ([ (\* self#statement *\) (S.exp b) ])
     ~else_:([self#statement (S.exp c)])
                 )
*)
module E = Js_exp_make
module S = Js_stmt_make

let super = Js_record_map.super

let flatten_map =
  {
    super with
    statement =
      (fun self x ->
        match x.statement_desc with
        | Exp ({expression_desc = Seq _; _} as v) ->
          S.block
            (List.rev_map
               (fun x -> self.statement self x)
               (Js_analyzer.rev_flatten_seq v))
        | Exp
            {
              expression_desc = Caml_block (args, _mutable_flag, _tag, _tag_info);
            } ->
          S.block
            (Ext_list.map args (fun arg -> self.statement self (S.exp arg)))
        | Exp {expression_desc = Cond (a, b, c); comment} ->
          {
            statement_desc =
              If
                ( a,
                  [self.statement self (S.exp b)],
                  [self.statement self (S.exp c)] );
            comment;
          }
        | Exp
            {
              expression_desc = Bin (Eq, a, ({expression_desc = Seq _; _} as v));
              _;
            } -> (
          let block = Js_analyzer.rev_flatten_seq v in
          match block with
          | {statement_desc = Exp last_one; _} :: rest_rev ->
            S.block
              (Ext_list.rev_map_append rest_rev
                 [self.statement self (S.exp (E.assign a last_one))]
                 (fun x -> self.statement self x))
          (* TODO: here we introduce a block, should avoid it *)
          (* super#statement *)
          (*   (S.block (List.rev_append rest_rev [S.exp (E.assign a  last_one)])) *)
          | _ -> assert false)
        | Return {expression_desc = Cond (a, b, c); comment} ->
          {
            statement_desc =
              If
                ( a,
                  [self.statement self (S.return_stmt b)],
                  [self.statement self (S.return_stmt c)] );
            comment;
          }
        | Return ({expression_desc = Seq _; _} as v) -> (
          let block = Js_analyzer.rev_flatten_seq v in
          match block with
          | {statement_desc = Exp last_one; _} :: rest_rev ->
            super.statement self
              (S.block
                 (Ext_list.rev_map_append rest_rev
                    [S.return_stmt last_one]
                    (fun x -> self.statement self x)))
          | _ -> assert false)
        | Block [x] -> self.statement self x
        | _ -> super.statement self x);
    block =
      (fun self b ->
        match b with
        | {statement_desc = Block bs} :: rest -> self.block self (bs @ rest)
        | x :: rest -> (
          let st = self.statement self x in
          let block = self.block self rest in
          match st.statement_desc with
          | Block bs -> bs @ block
          | _ -> st :: block)
        | [] -> []);
  }

let program (x : J.program) = flatten_map.program flatten_map x
