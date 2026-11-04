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

type arg_expression = Splice0 | Splice1 of E.t | Splice2 of E.t * E.t

(* Whether evaluating [e] may await, other than in a function it contains.
   Raw JS code may, unless it is a function or a literal: the compiler does not
   see what else it does. *)
let awaits (e : J.expression) =
  let super = Js_record_iter.super in
  let iter =
    {
      super with
      expression =
        (fun self e ->
          match e.expression_desc with
          | Await _ | Raw_js_code {code_info = Exp Js_exp_unknown | Stmt _} ->
            raise_notrace Exit
          | Fun _ | Raw_js_code {code_info = Exp (Js_function _ | Js_literal _)}
            ->
            ()
          | _ -> super.expression self e);
    }
  in
  match iter.expression iter e with
  | () -> false
  | exception Exit -> true

(* [eval] and [eval_as_int] read an argument other than a literal once,
   inside an arrow function that dispatches on it and is called where the
   argument is. That is the same as evaluating the argument in place, side
   effects included, since an arrow function shares [this] and [arguments]
   with the code around it, and an expression cannot [break], [continue] or
   [return] other than in a function of its own. Only an [await] cannot move
   there: it is a syntax error in the arrow function. *)
let eval_in_place (arg : J.expression) = not (awaits arg)

(* [eval_as_event] reads an argument other than a literal twice, for the tag
   (inside the function that dispatches on it, with [@as]) and for the
   payload. That is the same as reading it once, in place, for constants,
   variables, and fields, array elements and conditionals of them. A JS global
   ([@val]) counts as a variable, like a value of a JS module ([@module]),
   though either may be a getter. A JS property read does not count: a [@get]
   external is there to run a getter, and a [@val] or [@module] value with
   [@scope] is a property read as well, which cannot be told from it. *)
let eval_as_event_in_place (arg : J.expression) =
  let rec duplicable (e : J.expression) =
    match e.expression_desc with
    | Undefined _ | Bool _ | Number _ | Str _ | Template_literal _ | Var _ ->
      true
    | Static_index (e, _, Some _) -> duplicable e
    | Array_index (a, b) -> duplicable a && duplicable b
    | Cond (a, b, c) -> duplicable a && duplicable b && duplicable c
    | _ -> false
  in
  match arg.expression_desc with
  | Caml_block ([{expression_desc = Str _}; _], _, Blk_poly_var) -> true
  | _ -> duplicable arg

(* we need destruct [undefined] when input is optional *)
let eval (arg : J.expression) (dispatches : (string * string) list) : E.t =
  if arg == E.undefined then E.undefined
  else
    match arg.expression_desc with
    | Str txt ->
      let s = Ext_list.assoc_by_string dispatches txt None in
      E.str s
    | _ ->
      E.of_block
        [
          S.string_switch arg
            (Ext_list.map dispatches (fun (s, r) ->
                 ( Variant_runtime.Literal (String s),
                   J.
                     {
                       switch_body = [S.return_stmt (E.str r)];
                       should_break = false;
                       (* FIXME: if true, still print break*)
                       comment = None;
                       source_loc = None;
                     } )));
        ]

(* invariant: optional is not allowed in this case *)
(* arg is a polyvar *)

(** A literal passes its tag and its payload, evaluated in place. Any other
    [arg] is read twice, for the tag and for the payload, so the caller binds
    it to a variable first unless it is [eval_as_event_in_place]. *)
let eval_as_event (arg : J.expression)
    (dispatches : (string * string) list option) =
  match arg.expression_desc with
  | Caml_block ([{expression_desc = Str txt}; cb], _, Blk_poly_var) ->
    let v =
      match dispatches with
      | Some dispatches -> Ext_list.assoc_by_string dispatches txt None
      | None -> txt
    in
    Splice2 (E.str v, cb)
  | _ ->
    Splice2
      ( (match dispatches with
        | Some dispatches ->
          E.of_block
            [
              S.string_switch
                (E.poly_var_tag_access arg)
                (Ext_list.map dispatches (fun (s, r) ->
                     ( Variant_runtime.Literal (String s),
                       J.
                         {
                           switch_body = [S.return_stmt (E.str r)];
                           should_break = false;
                           (* FIXME: if true, still print break*)
                           comment = None;
                           source_loc = None;
                         } )));
            ]
        | None -> E.poly_var_tag_access arg),
        E.poly_var_value_access arg )

(* we need destruct [undefined] when input is optional *)
let eval_as_int (arg : J.expression) (dispatches : (string * int) list) : E.t =
  if arg == E.undefined then E.undefined
  else
    match arg.expression_desc with
    | Str txt ->
      E.int (Int32.of_int (Ext_list.assoc_by_string dispatches txt None))
    | _ ->
      E.of_block
        [
          S.string_switch arg
            (Ext_list.map dispatches (fun (s, r) ->
                 ( Variant_runtime.Literal (String s),
                   J.
                     {
                       switch_body = [S.return_stmt (E.int (Int32.of_int r))];
                       should_break = false;
                       (* FIXME: if true, still print break*)
                       comment = None;
                       source_loc = None;
                     } )));
        ]

let eval_as_unwrap (arg : J.expression) : E.t =
  match arg.expression_desc with
  | Caml_block ([{expression_desc = Number _}; cb], _, _) -> cb
  | _ -> E.poly_var_value_access arg
