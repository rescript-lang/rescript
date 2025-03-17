(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2016 - Hongbo Zhang, Authors of ReScript
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

type idents_stats = {
  mutable used_idents: Set_ident.t;
  mutable defined_idents: Set_ident.t;
}

let add_defined_idents (x : idents_stats) ident =
  x.defined_idents <- Set_ident.add x.defined_idents ident

(* Assume that functions already calculated closure correctly
   Maybe in the future, we should add a dirty flag, to mark the calcuated
   closure is correct or not

   Note such shaking is done in the toplevel, so that it requires us to
   flatten the statement first
*)
let super = Js_record_iter.super

let free_variables (stats : idents_stats) =
  {
    super with
    variable_declaration =
      (fun self st ->
        add_defined_idents stats st.ident;
        match st.value with
        | None -> ()
        | Some v -> self.expression self v);
    ident =
      (fun _ id ->
        if not (Set_ident.mem stats.defined_idents id) then
          stats.used_idents <- Set_ident.add stats.used_idents id);
    expression =
      (fun self exp ->
        match exp.expression_desc with
        | Fun {env}
        (* a optimization to avoid walking into function again
            if it's already comuted
        *)
          ->
          stats.used_idents <-
            Set_ident.union (Js_fun_env.get_unbounded env) stats.used_idents
        | _ -> super.expression self exp);
  }

let init = {used_idents = Set_ident.empty; defined_idents = Set_ident.empty}

let obj = free_variables init

let clean_up init =
  init.used_idents <- Set_ident.empty;
  init.defined_idents <- Set_ident.empty

let free_variables_of_statement st =
  clean_up init;
  obj.statement obj st;
  Set_ident.diff init.used_idents init.defined_idents

let free_variables_of_expression st =
  clean_up init;
  obj.expression obj st;
  Set_ident.diff init.used_idents init.defined_idents

let rec no_side_effect_expression_desc (x : J.expression_desc) =
  match x with
  | Undefined _ | Null | Bool _ | Var _ -> true
  | Fun _ -> true
  | Number _ -> true (* Can be refined later *)
  | Static_index (obj, (_name : string), (_pos : int32 option)) ->
    no_side_effect obj
  | String_index (a, b) | Array_index (a, b) ->
    no_side_effect a && no_side_effect b
  | Is_null_or_undefined b -> no_side_effect b
  | Str _ -> true
  | Array (xs, _mutable_flag) | Caml_block (xs, _mutable_flag, _, _) ->
    (* create [immutable] block,
        does not really mean that this opreation itself is [pure].

        the block is mutable does not mean this operation is non-pure
    *)
    Ext_list.for_all xs no_side_effect
  | Optional_block (x, _) -> no_side_effect x
  | Object (_, kvs) -> Ext_list.for_all_snd kvs no_side_effect
  | String_append (a, b) | Seq (a, b) -> no_side_effect a && no_side_effect b
  | Length (e, _) | Caml_block_tag (e, _) | Typeof e -> no_side_effect e
  | Bin (op, a, b) -> op <> Eq && no_side_effect a && no_side_effect b
  | Tagged_template (call_expr, strings, values) ->
    no_side_effect call_expr
    && Ext_list.for_all strings no_side_effect
    && Ext_list.for_all values no_side_effect
  | Js_not e -> no_side_effect e
  | Cond (a, b, c) -> no_side_effect a && no_side_effect b && no_side_effect c
  | Call ({expression_desc = Str {txt = "Array.isArray"}}, [e], _) ->
    no_side_effect e
  | FlatCall _ | Call _ | New _ | Raw_js_code _ (* actually true? *) -> false
  | Await _ -> false
  | Spread _ -> false

and no_side_effect (x : J.expression) =
  no_side_effect_expression_desc x.expression_desc

let no_side_effect_expression (x : J.expression) = no_side_effect x

let super = Js_record_iter.super

let no_side_effect_obj =
  {
    super with
    statement =
      (fun self s ->
        match s.statement_desc with
        | Throw _ | Debugger | Break | Variable _ | Continue ->
          raise_notrace Not_found
        | Exp e -> self.expression self e
        | Int_switch _ | String_switch _ | ForRange _ | If _ | While _ | Block _
        | Return _ | Try _ ->
          super.statement self s);
    expression =
      (fun _ s ->
        if not (no_side_effect_expression s) then raise_notrace Not_found);
  }

let no_side_effect_statement st =
  try
    no_side_effect_obj.statement no_side_effect_obj st;
    true
  with _ -> false

(* TODO: generate [fold2]
   This make sense, for example:
   {[
     let string_of_formatting_gen : type a b c d e f .
       (a, b, c, d, e, f) formatting_gen -> string =
       fun formatting_gen -> match formatting_gen with
         | Open_tag (Format (_, str)) -> str
         | Open_box (Format (_, str)) -> str

   ]}
*)
let rec eq_expression ({expression_desc = x0} : J.expression)
    ({expression_desc = y0} : J.expression) =
  match x0 with
  | Null -> y0 = Null
  | Undefined x -> y0 = Undefined x
  | Number (Int {i}) -> (
    match y0 with
    | Number (Int {i = j}) -> i = j
    | _ -> false)
  | Number (BigInt {positive = p0; value = v0}) -> (
    match y0 with
    | Number (BigInt {positive = p1; value = v1}) -> p0 = p1 && v0 = v1
    | _ -> false)
  | Number (Float _) -> false
  (* begin match y0 with
     | Number (Float j) ->
      false (* conservative *)
     | _ -> false
     end *)
  | String_index (a0, a1) -> (
    match y0 with
    | String_index (b0, b1) -> eq_expression a0 b0 && eq_expression a1 b1
    | _ -> false)
  | Array_index (a0, a1) -> (
    match y0 with
    | Array_index (b0, b1) -> eq_expression a0 b0 && eq_expression a1 b1
    | _ -> false)
  | Call (a0, args00, _) -> (
    match y0 with
    | Call (b0, args10, _) ->
      eq_expression a0 b0 && eq_expression_list args00 args10
    | _ -> false)
  | Var x -> (
    match y0 with
    | Var y -> Js_op_util.same_vident x y
    | _ -> false)
  | Bin (op0, a0, b0) -> (
    match y0 with
    | Bin (op1, a1, b1) ->
      op0 = op1 && eq_expression a0 a1 && eq_expression b0 b1
    | _ -> false)
  | Str {delim = a0; txt = b0} -> (
    match y0 with
    | Str {delim = a1; txt = b1} -> a0 = a1 && b0 = b1
    | _ -> false)
  | Static_index (e0, p0, off0) -> (
    match y0 with
    | Static_index (e1, p1, off1) ->
      p0 = p1 && eq_expression e0 e1 && off0 = off1 (* could be relaxed *)
    | _ -> false)
  | Seq (a0, b0) -> (
    match y0 with
    | Seq (a1, b1) -> eq_expression a0 a1 && eq_expression b0 b1
    | _ -> false)
  | Bool a0 -> (
    match y0 with
    | Bool b0 -> a0 = b0
    | _ -> false)
  | Optional_block (a0, b0) -> (
    match y0 with
    | Optional_block (a1, b1) -> b0 = b1 && eq_expression a0 a1
    | _ -> false)
  | Caml_block (ls0, flag0, tag0, _) -> (
    match y0 with
    | Caml_block (ls1, flag1, tag1, _) ->
      eq_expression_list ls0 ls1 && flag0 = flag1 && eq_expression tag0 tag1
    | _ -> false)
  | Length _ | Is_null_or_undefined _ | String_append _ | Typeof _ | Js_not _
  | Cond _ | FlatCall _ | New _ | Fun _ | Raw_js_code _ | Array _
  | Caml_block_tag _ | Object _ | Tagged_template _ | Await _ ->
    false
  | Spread _ -> false

and eq_expression_list xs ys = Ext_list.for_all2_no_exn xs ys eq_expression

and eq_block (xs : J.block) (ys : J.block) =
  Ext_list.for_all2_no_exn xs ys eq_statement

and eq_statement ({statement_desc = x0} : J.statement)
    ({statement_desc = y0} : J.statement) =
  match x0 with
  | Exp a -> (
    match y0 with
    | Exp b -> eq_expression a b
    | _ -> false)
  | Return a -> (
    match y0 with
    | Return b -> eq_expression a b
    | _ -> false)
  | Debugger -> y0 = Debugger
  | Break -> y0 = Break
  | Block xs0 -> (
    match y0 with
    | Block ys0 -> eq_block xs0 ys0
    | _ -> false)
  | Variable _ | If _ | While _ | ForRange _ | Continue | Int_switch _
  | String_switch _ | Throw _ | Try _ ->
    false

let rev_flatten_seq (x : J.expression) =
  let rec aux acc (x : J.expression) : J.block =
    match x.expression_desc with
    | Seq (a, b) -> aux (aux acc a) b
    | _ -> {statement_desc = Exp x; comment = None} :: acc
  in
  aux [] x

(* TODO: optimization,
    counter the number to know if needed do a loop gain instead of doing a diff
*)

let rev_toplevel_flatten block =
  let rec aux acc (xs : J.block) : J.block =
    match xs with
    | [] -> acc
    | {
        statement_desc =
          Variable
            ( {ident_info = {used_stats = Dead_pure}; _}
            | {ident_info = {used_stats = Dead_non_pure}; value = None} );
      }
      :: xs ->
      aux acc xs
    | {statement_desc = Block b; _} :: xs -> aux (aux acc b) xs
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] block

(* let rec is_constant (x : J.expression)  =
   match x.expression_desc with
   | Array_index (a,b) -> is_constant a && is_constant b
   | Str (b,_) -> b
   | Number _ -> true (* Can be refined later *)
   | Array (xs,_mutable_flag)  -> Ext_list.for_all xs  is_constant
   | Caml_block(xs, Immutable, tag, _)
    -> Ext_list.for_all xs is_constant && is_constant tag
   | Bin (_op, a, b) ->
    is_constant a && is_constant b
   | _ -> false *)

let rec is_okay_to_duplicate (e : J.expression) =
  match e.expression_desc with
  | Var _ | Bool _ | Str _ | Number _ -> true
  | Static_index (e, _s, _off) -> is_okay_to_duplicate e
  | _ -> false
