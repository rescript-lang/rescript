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

(* [eval_as_event] reads an argument other than a literal twice, for the tag
   and for the payload. That is the same as reading it once, in place, when
   [Js_analyzer.is_okay_to_duplicate ~property_reads:false] holds for it: for
   constants, variables and fields of records. A JS global ([@val]) counts as
   a variable, like a value of a JS module ([@module]), though either may be
   a getter. A JS property read does not count: a [@get] external is there to
   run a getter, and a [@val] or [@module] value with [@scope], or a [@val]
   with a dotted name such as ["window.ev"], is a property read as well,
   which cannot be told from it. Array elements and conditionals of such
   arguments, and their fields, count too, though [is_okay_to_duplicate]
   leaves them out: here the alternative is a temporary, a statement that
   also binds the arguments to its left that have side effects. *)
let eval_as_event_in_place (arg : J.expression) =
  let rec duplicable (e : J.expression) =
    match e.expression_desc with
    | Static_index (e, _, Some _) -> duplicable e
    | Array_index (a, b) -> duplicable a && duplicable b
    | Cond (a, b, c) -> duplicable a && duplicable b && duplicable c
    | _ -> Js_analyzer.is_okay_to_duplicate ~property_reads:false e
  in
  match arg.expression_desc with
  | Caml_block ([{expression_desc = Str _}; _], _, Blk_poly_var) -> true
  | _ -> duplicable arg

(* [(x => { switch (x) { case s: return r; ... } })(arg)]: the result [r] for
   the case [s] that [arg] matches. [arg] is an argument of the call, so it is
   evaluated once and in its place, and an [await] in it belongs to the code
   around it. The function captures no variable of that code, so it keeps
   none of them alive for the closures that code creates. *)
let dispatch (arg : J.expression) (cases : (string * 'a) list)
    (result : 'a -> J.expression) : J.expression =
  let x = Ident.create "x" in
  E.call ~info:Js_call_info.ml_full_call
    (E.ocaml_fun ~return_unit:false ~async:false ~one_unit_arg:false [x]
       [
         S.string_switch (E.var x)
           (Ext_list.map cases (fun (s, r) ->
                ( Variant_runtime.Literal (String s),
                  J.
                    {
                      switch_body = [S.return_stmt (result r)];
                      should_break = false;
                      (* FIXME: if true, still print break*)
                      comment = None;
                      source_loc = None;
                    } )));
       ])
    [arg]

(* we need destruct [undefined] when input is optional *)
let eval (arg : J.expression) (dispatches : (string * string) list) : E.t =
  if arg == E.undefined then E.undefined
  else
    match arg.expression_desc with
    | Str txt ->
      let s = Ext_list.assoc_by_string dispatches txt None in
      E.str s
    | _ -> dispatch arg dispatches E.str

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
          dispatch (E.poly_var_tag_access arg) dispatches E.str
        | None -> E.poly_var_tag_access arg),
        E.poly_var_value_access arg )

(* we need destruct [undefined] when input is optional *)
let eval_as_int (arg : J.expression) (dispatches : (string * int) list) : E.t =
  if arg == E.undefined then E.undefined
  else
    match arg.expression_desc with
    | Str txt ->
      E.int (Int32.of_int (Ext_list.assoc_by_string dispatches txt None))
    | _ -> dispatch arg dispatches (fun r -> E.int (Int32.of_int r))

let eval_as_unwrap (arg : J.expression) : E.t =
  match arg.expression_desc with
  | Caml_block ([{expression_desc = Number _}; cb], _, _) -> cb
  | _ -> E.poly_var_value_access arg
