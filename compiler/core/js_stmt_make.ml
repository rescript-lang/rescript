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

type t = J.statement

let return_stmt ?comment e : t = {statement_desc = Return e; comment}

let empty_stmt : t = {statement_desc = Block []; comment = None}

(* let empty_block : J.block = [] *)
let throw_stmt ?comment v : t = {statement_desc = Throw v; comment}

(* avoid nested block *)
let rec block ?comment (b : J.block) : t =
  match b with
  | [{statement_desc = Block bs}] -> block bs
  | [b] -> b
  | [] -> empty_stmt
  | _ -> {statement_desc = Block b; comment}

(* It's a statement, we can discard some values *)
let rec exp ?comment (e : E.t) : t =
  match e.expression_desc with
  | Seq ({expression_desc = Number _ | Undefined _}, b)
  | Seq (b, {expression_desc = Number _ | Undefined _}) ->
    exp ?comment b
  | Number _ | Undefined _ -> block []
  (* TODO: we can do more *)
  (* | _ when is_pure e ->  block [] *)
  | _ -> {statement_desc = Exp e; comment}

let declare_variable ?comment ?ident_info ~kind (ident : Ident.t) : t =
  let property : J.property = kind in
  let ident_info : J.ident_info =
    match ident_info with
    | None -> {used_stats = NA}
    | Some x -> x
  in
  {
    statement_desc = Variable {ident; value = None; property; ident_info};
    comment;
  }

let define_variable ?comment ?ident_info ~kind (v : Ident.t)
    (exp : J.expression) : t =
  match exp.expression_desc with
  | Undefined _ -> declare_variable ?comment ?ident_info ~kind v
  | _ ->
    let property : J.property = kind in
    let ident_info : J.ident_info =
      match ident_info with
      | None -> {used_stats = NA}
      | Some x -> x
    in
    {
      statement_desc =
        Variable {ident = v; value = Some exp; property; ident_info};
      comment;
    }

(* let alias_variable ?comment  ~exp (v:Ident.t)  : t=
   {statement_desc =
     Variable {
       ident = v; value = Some exp; property = Alias;
       ident_info = {used_stats = NA }   };
   comment} *)

let int_switch ?(comment : string option)
    ?(declaration : (J.property * Ident.t) option) ?(default : J.block option)
    (e : J.expression) (clauses : (int * J.case_clause) list) : t =
  match e.expression_desc with
  | Number (Int {i; _}) -> (
    let continuation =
      match
        Ext_list.find_opt clauses (fun (switch_case, x) ->
            if switch_case = Int32.to_int i then Some x.switch_body else None)
      with
      | Some case -> case
      | None -> (
        match default with
        | Some x -> x
        | None -> assert false)
    in
    match (declaration, continuation) with
    | ( Some (kind, did),
        [
          {
            statement_desc =
              Exp
                {
                  expression_desc =
                    Bin (Eq, {expression_desc = Var (Id id); _}, e0);
                  _;
                };
            _;
          };
        ] )
      when Ident.same did id ->
      define_variable ?comment ~kind id e0
    | Some (kind, did), _ ->
      block (declare_variable ?comment ~kind did :: continuation)
    | None, _ -> block continuation)
  | _ -> (
    match declaration with
    | Some (kind, did) ->
      block
        [
          declare_variable ?comment ~kind did;
          {statement_desc = J.Int_switch (e, clauses, default); comment};
        ]
    | None -> {statement_desc = J.Int_switch (e, clauses, default); comment})

let string_switch ?(comment : string option)
    ?(declaration : (J.property * Ident.t) option) ?(default : J.block option)
    (e : J.expression)
    (clauses : (Ast_untagged_variants.tag_type * J.case_clause) list) : t =
  match e.expression_desc with
  | Str {txt} -> (
    let continuation =
      match
        Ext_list.find_opt clauses (fun (switch_case, x) ->
            match switch_case with
            | String s -> if s = txt then Some x.switch_body else None
            | Int _ | Float _ | BigInt _ | Bool _ | Null | Undefined
            | Untagged _ ->
              None)
      with
      | Some case -> case
      | None -> (
        match default with
        | Some x -> x
        | None -> assert false)
    in
    match (declaration, continuation) with
    | ( Some (kind, did),
        [
          {
            statement_desc =
              Exp
                {
                  expression_desc =
                    Bin (Eq, {expression_desc = Var (Id id); _}, e0);
                  _;
                };
            _;
          };
        ] )
      when Ident.same did id ->
      define_variable ?comment ~kind id e0
    | Some (kind, did), _ ->
      block @@ (declare_variable ?comment ~kind did :: continuation)
    | None, _ -> block continuation)
  | _ -> (
    match declaration with
    | Some (kind, did) ->
      block
        [
          declare_variable ?comment ~kind did;
          {statement_desc = String_switch (e, clauses, default); comment};
        ]
    | None -> {statement_desc = String_switch (e, clauses, default); comment})

let rec block_last_is_return_throw_or_continue (x : J.block) =
  match x with
  | [] -> false
  | [x] -> (
    match x.statement_desc with
    | Return _ | Throw _ | Continue -> true
    | _ -> false)
  | _ :: rest -> block_last_is_return_throw_or_continue rest

(* TODO: it also make sense  to extract some common statements
    between those two branches, it does happen since in OCaml you
    have to write some duplicated code due to the types system restriction
    example:
   {[
     | Format_subst (pad_opt, fmtty, rest) ->
       buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
       bprint_pad_opt buf pad_opt; buffer_add_char buf '(';
       bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf ')';
       fmtiter rest false;

       | Scan_char_set (width_opt, char_set, rest) ->
       buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
       bprint_pad_opt buf width_opt; bprint_char_set buf char_set;
       fmtiter rest false;
   ]}

   To hit this branch, we also need [declaration] passed down
           TODO: check how we compile [Lifthenelse]
    The declaration argument is introduced to merge assignment in both branches

   Note we can transfer code as below:
   {[
     if (x){
         return /throw e;
       } else {
     blabla
   }
   ]}
   into
   {[
     if (x){
         return /throw e;
       }
         blabla
   ]}
   Not clear the benefit
*)
let if_ ?comment ?declaration ?else_ (e : J.expression) (then_ : J.block) : t =
  let declared = ref false in
  let rec aux ?comment (e : J.expression) (ifso : J.block) (ifnot : J.block) : t
      =
    match (e.expression_desc, ifnot) with
    | Bool boolean, _ -> block (if boolean then ifso else ifnot)
    | Js_not pred_not, _ :: _ -> aux ?comment pred_not ifnot ifso
    | _ -> (
      match (ifso, ifnot) with
      | [], [] -> exp e
      | [], _ -> aux ?comment (E.not e) ifnot [] (*Make sure no infinite loop*)
      | ( [{statement_desc = Return ret_ifso; _}],
          [{statement_desc = Return ret_ifnot; _}] ) ->
        return_stmt (E.econd e ret_ifso ret_ifnot)
      | _, [{statement_desc = Return _}] ->
        block ({statement_desc = If (E.not e, ifnot, []); comment} :: ifso)
      | _, _ when block_last_is_return_throw_or_continue ifso ->
        block ({statement_desc = If (e, ifso, []); comment} :: ifnot)
      | ( [
            {
              statement_desc =
                Exp
                  {
                    expression_desc =
                      Bin
                        ( Eq,
                          ({expression_desc = Var (Id var_ifso); _} as lhs_ifso),
                          rhs_ifso );
                    _;
                  };
              _;
            };
          ],
          [
            {
              statement_desc =
                Exp
                  {
                    expression_desc =
                      Bin
                        ( Eq,
                          {expression_desc = Var (Id var_ifnot); _},
                          lhs_ifnot );
                    _;
                  };
              _;
            };
          ] )
        when Ident.same var_ifso var_ifnot -> (
        match declaration with
        | Some (kind, id) when Ident.same id var_ifso ->
          declared := true;
          define_variable ~kind var_ifso (E.econd e rhs_ifso lhs_ifnot)
        | _ -> exp (E.assign lhs_ifso (E.econd e rhs_ifso lhs_ifnot)))
      | ( [{statement_desc = Exp exp_ifso; _}],
          [{statement_desc = Exp exp_ifnot; _}] ) ->
        exp (E.econd e exp_ifso exp_ifnot)
      | [{statement_desc = If (pred1, ifso1, ifnot1)}], _
        when Js_analyzer.eq_block ifnot1 ifnot ->
        aux ?comment (E.and_ e pred1) ifso1 ifnot1
      | [{statement_desc = If (pred1, ifso1, ifnot1)}], _
        when Js_analyzer.eq_block ifso1 ifnot ->
        aux ?comment (E.and_ e (E.not pred1)) ifnot1 ifso1
      | _, [{statement_desc = If (pred1, ifso1, else_)}]
        when Js_analyzer.eq_block ifso ifso1 ->
        aux ?comment (E.or_ e pred1) ifso else_
      | _, [{statement_desc = If (pred1, ifso1, ifnot1)}]
        when Js_analyzer.eq_block ifso ifnot1 ->
        aux ?comment (E.or_ e (E.not pred1)) ifso ifso1
      | _ -> {statement_desc = If (e, ifso, ifnot); comment})
  in
  let if_block =
    aux ?comment e then_
      (match else_ with
      | None -> []
      | Some v -> v)
  in
  match (!declared, declaration) with
  | true, _ | _, None -> if_block
  | false, Some (kind, id) -> block (declare_variable ~kind id :: [if_block])

let assign ?comment id e : t =
  {statement_desc = J.Exp (E.assign (E.var id) e); comment}

let while_ ?comment (e : E.t) (st : J.block) : t =
  {statement_desc = While (e, st); comment}

let for_ ?comment for_ident_expression finish_ident_expression id direction
    (b : J.block) : t =
  {
    statement_desc =
      ForRange (for_ident_expression, finish_ident_expression, id, direction, b);
    comment;
  }

let try_ ?comment ?with_ ?finally body : t =
  {statement_desc = Try (body, with_, finally); comment}

let continue_ : t = {statement_desc = Continue; comment = None}

let debugger_block : t list = [{statement_desc = Debugger; comment = None}]
