(* ReScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(* Authors: Jérôme Vouillon, Hongbo Zhang  *)

(*
  http://stackoverflow.com/questions/2846283/what-are-the-rules-for-javascripts-automatic-semicolon-insertion-asi
  ASI catch up
   {[
     a=b
       ++c
       ---
       a=b ++c
     ====================
     a ++
     ---
     a
     ++
     ====================
     a --
     ---
     a
     --
     ====================
     (continue/break/return/throw) a
     ---
     (continue/break/return/throw)
       a
     ====================
   ]}

*)

let name_symbol = Js_op.Symbol_name

module P = Ext_pp
module E = Js_exp_make
module S = Js_stmt_make
module L = Js_dump_lit

(* There modules are dynamically inserted in the last stage
   {Caml_curry}
   {Caml_option}

   They can appear anywhere so even if you have a module
   {
    let module Caml_block = ...

    (* Later would insert the use of Caml_block here which should
      point tto the runtime module
    *)
   }
   There are no sane way to easy detect it ahead of time, we should be
   conservative here.
   (our call Js_fun_env.get_unbounded env) is not precise
*)

module Curry_gen = struct
  let pp_curry_dot f =
    P.string f Primitive_modules.curry;
    P.string f L.dot

  let pp_optimize_curry (f : P.t) (len : int) =
    pp_curry_dot f;
    P.string f "__";
    P.string f (Printf.sprintf "%d" len)

  let pp_app_any (f : P.t) =
    pp_curry_dot f;
    P.string f "app"

  let pp_app (f : P.t) (len : int) =
    pp_curry_dot f;
    P.string f "_";
    P.string f (Printf.sprintf "%d" len)
end

type cxt = Ext_pp_scope.t

let semi f = P.string f L.semi
let comma f = P.string f L.comma

let exn_block_as_obj ~(stack : bool) (el : J.expression list) (ext : J.tag_info)
    : J.expression_desc =
  let field_name =
    match ext with
    | Blk_extension -> (
      fun i ->
        match i with
        | 0 -> Literals.exception_id
        | i -> "_" ^ string_of_int i)
    | Blk_record_ext {fields = ss} -> (
      fun i ->
        match i with
        | 0 -> Literals.exception_id
        | i -> ss.(i - 1))
    | _ -> assert false
  in
  Object
    ( None,
      if stack then
        Ext_list.mapi_append el
          (fun i e -> (Js_op.Lit (field_name i), e))
          [(Js_op.Lit "Error", E.new_ (E.js_global "Error") [])]
      else Ext_list.mapi el (fun i e -> (Js_op.Lit (field_name i), e)) )

let rec iter_lst cxt (f : P.t) ls element inter =
  match ls with
  | [] -> cxt
  | [e] -> element cxt f e
  | e :: r ->
    let acxt = element cxt f e in
    inter f;
    iter_lst acxt f r element inter

let raw_snippet_exp_simple_enough (s : string) =
  Ext_string.for_all s (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' -> true
      | _ -> false)
(* Parentheses are required when the expression
   starts syntactically with "{" or "function"
   TODO:  be more conservative, since Google Closure will handle
   the precedence correctly, we also need people read the code..
   Here we force parens for some alien operators

   If we move assign into a statement, will be less?
   TODO: construct a test case that do need parenthesisze for expression
   IIE does not apply (will be inlined?)
*)

(* e = function(x){...}(x);  is good
*)
let rec exp_need_paren ?(arrow = false) (e : J.expression) =
  match e.expression_desc with
  (* | Caml_uninitialized_obj _  *)
  | Call ({expression_desc = Raw_js_code _}, _, _) -> true
  | Raw_js_code {code_info = Exp _}
  | Fun _
  | Caml_block
      ( _,
        _,
        _,
        ( Blk_record _ | Blk_module _ | Blk_poly_var _ | Blk_extension
        | Blk_record_ext _ | Blk_record_inlined _ | Blk_constructor _ ) )
  | Object _ ->
    true
  | Raw_js_code {code_info = Stmt _}
  | Length _ | Call _ | Caml_block_tag _ | Seq _ | Static_index _ | Cond _
  | Bin _ | Is_null_or_undefined _ | String_index _ | Array_index _
  | String_append _ | Var _ | Undefined _ | Null | Str _ | Array _
  | Caml_block _ | FlatCall _ | Typeof _ | Number _ | Js_not _ | Bool _ | New _
    ->
    false
  | Await _ -> false
  | Spread _ -> false
  | Tagged_template _ -> false
  | Optional_block (e, true) when arrow -> exp_need_paren ~arrow e
  | Optional_block _ -> false

(** Print as underscore for unused vars, may not be 
    needed in the future *)
(* let ipp_ident cxt f id (un_used : bool) =
   Ext_pp_scope.ident cxt f (
    if un_used then
      Ext_ident.make_unused ()
    else
      id) *)

let pp_var_assign cxt f id =
  P.string f L.let_;
  P.space f;
  let acxt = Ext_pp_scope.ident cxt f id in
  P.space f;
  P.string f L.eq;
  P.space f;
  acxt

let pp_var_assign_this cxt f id =
  let cxt = pp_var_assign cxt f id in
  P.string f L.this;
  P.space f;
  semi f;
  P.newline f;
  cxt

let pp_var_declare cxt f id =
  P.string f L.let_;
  P.space f;
  let acxt = Ext_pp_scope.ident cxt f id in
  semi f;
  acxt

let pp_direction f (direction : J.for_direction) =
  match direction with
  | Up | Upto -> P.string f L.plus_plus
  | Downto -> P.string f L.minus_minus

let return_sp f =
  P.string f L.return;
  P.space f

let bool f b = P.string f (if b then L.true_ else L.false_)

let comma_sp f =
  comma f;
  P.space f

let comma_nl f =
  comma f;
  P.newline f

(* let drop_comment (x : J.expression) =
   if x.comment = None then x
   else {x with comment = None} *)

let debugger_nl f =
  P.newline f;
  P.string f L.debugger;
  semi f;
  P.newline f

let break_nl f =
  P.string f L.break;
  semi f;
  P.newline f

let continue f =
  P.string f L.continue;
  semi f

let formal_parameter_list cxt f l = iter_lst cxt f l Ext_pp_scope.ident comma_sp

(* IdentMap *)
(*
f/122 -->
  f/122 is in the map
  if in, use the old mapping
  else
    check  f,
     if in last bumped id
     else
        use "f", register it

  check "f"
         if not , use "f", register stamp -> 0
         else
           check stamp
             if in  use it
             else check last bumped id, increase it and register
*)

(**
   Turn [function f (x,y) { return a (x,y)} ] into [Curry.__2(a)],
   The idea is that [Curry.__2] will guess the arity of [a], if it does
   hit, then there is no cost when passed
*)

let is_var (b : J.expression) a =
  match b.expression_desc with
  | Var (Id i) -> Ident.same i a
  | _ -> false

type fn_exp_state =
  | Is_return (* for sure no name *)
  | Name_top of Ident.t
  | Name_non_top of Ident.t
  | No_name of {single_arg: bool}
(* true means for sure, false -- not sure *)

let default_fn_exp_state = No_name {single_arg = false}

(* TODO: refactoring
   Note that {!pp_function} could print both statement and expression when [No_name] is given
*)
let rec try_optimize_curry cxt f len function_id =
  Curry_gen.pp_optimize_curry f len;
  P.paren_group f 1 (fun _ -> expression ~level:1 cxt f function_id)

and pp_function ~return_unit ~async ~is_method ?directive cxt (f : P.t)
    ~fn_state (l : Ident.t list) (b : J.block) (env : Js_fun_env.t) : cxt =
  match b with
  | [
   {
     statement_desc =
       Return
         {
           expression_desc =
             Call
               ( ({expression_desc = Var v; _} as function_id),
                 ls,
                 {
                   arity = (Full | NA) as arity (* see #234*);
                   (* TODO: need a case to justify it*)
                   call_info = Call_builtin_runtime | Call_ml;
                 } );
         };
   };
  ]
    when (* match such case:
            {[ function(x,y){ return u(x,y) } ]}
            it can be optimized in to either [u] or [Curry.__n(u)]
         *)
         (not is_method)
         && Ext_list.for_all2_no_exn ls l is_var
         &&
         match v with
         (* This check is needed to avoid some edge cases
            {[function(x){return x(x)}]}
            here the function is also called `x`
         *)
         | Id id -> not (Ext_list.exists l (fun x -> Ident.same x id))
         | Qualified _ -> true -> (
    let optimize len ~p cxt f v =
      if p then try_optimize_curry cxt f len function_id else vident cxt f v
    in
    let len = List.length l in
    (* length *)
    match fn_state with
    | Name_top i | Name_non_top i ->
      let cxt = pp_var_assign cxt f i in
      let cxt = optimize len ~p:(arity = NA && len <= 8) cxt f v in
      semi f;
      cxt
    | Is_return | No_name _ ->
      if fn_state = Is_return then return_sp f;
      optimize len ~p:(arity = NA && len <= 8) cxt f v)
  | _ ->
    let set_env : Set_ident.t =
      (* identifiers will be printed following*)
      match fn_state with
      | Is_return | No_name _ -> Js_fun_env.get_unbounded env
      | Name_top id | Name_non_top id ->
        Set_ident.add (Js_fun_env.get_unbounded env) id
    in
    (* the context will be continued after this function *)
    let outer_cxt = Ext_pp_scope.merge cxt set_env in

    (* whether the function output can use arrow syntax *)
    let arrow =
      match fn_state with
      | Name_top _ -> false
      | _ -> not is_method
    in

    (* the context used to be printed inside this function

       when printing a function,
       only the enclosed variables and function name matters,
       if the function does not capture any variable, then the context is empty
    *)
    let inner_cxt = Ext_pp_scope.sub_scope outer_cxt set_env in
    let param_body () : unit =
      if is_method then (
        match l with
        | [] -> assert false
        | this :: arguments ->
          let cxt =
            P.paren_group f 1 (fun _ ->
                formal_parameter_list inner_cxt f arguments)
          in
          P.space f;
          P.brace_vgroup f 1 (fun _ ->
              let cxt =
                if Js_fun_env.get_unused env 0 then cxt
                else pp_var_assign_this cxt f this
              in
              function_body ?directive ~return_unit cxt f b))
      else
        let cxt =
          match l with
          | [single] when arrow -> Ext_pp_scope.ident inner_cxt f single
          | l ->
            P.paren_group f 1 (fun _ -> formal_parameter_list inner_cxt f l)
        in
        P.space f;
        if arrow then (
          P.string f L.arrow;
          P.space f);
        match b with
        | [{statement_desc = Return {expression_desc = Undefined _}}] when arrow
          ->
          P.string f "{";
          P.string f "}"
        | ([{statement_desc = Return e}] | [{statement_desc = Exp e}])
          when arrow && directive == None ->
          (if exp_need_paren ~arrow e then P.paren_group f 0 else P.group f 0)
            (fun _ -> ignore (expression ~level:0 cxt f e))
        | _ ->
          P.brace_vgroup f 1 (fun _ ->
              function_body ?directive ~return_unit cxt f b)
    in
    let enclose () =
      let handle () =
        match fn_state with
        | Is_return ->
          return_sp f;
          P.string f (L.function_ ~async ~arrow);
          param_body ()
        | No_name _ ->
          P.string f (L.function_ ~async ~arrow);
          param_body ()
        | Name_non_top x ->
          ignore (pp_var_assign inner_cxt f x : cxt);
          P.string f (L.function_ ~async ~arrow);
          param_body ();
          semi f
        | Name_top x ->
          P.string f (L.function_ ~async ~arrow);
          ignore (Ext_pp_scope.ident inner_cxt f x : cxt);
          param_body ()
      in
      handle ()
    in
    enclose ();
    outer_cxt

(* Assume the cond would not change the context,
    since it can be either [int] or [string]
*)
and pp_one_case_clause :
    'a. _ -> P.t -> (P.t -> 'a -> unit) -> 'a * J.case_clause -> _ =
 fun cxt f pp_cond
     (switch_case, ({switch_body; should_break; comment} : J.case_clause)) ->
  P.newline f;
  let cxt =
    P.group f 1 (fun _ ->
        P.group f 0 (fun _ ->
            P.string f L.case;
            P.space f;
            pp_comment_option f comment;
            pp_cond f switch_case;
            (* could be integer or string *)
            P.space f;
            P.string f L.colon);
        P.group f 0 (fun _ ->
            let cxt =
              match switch_body with
              | [] -> cxt
              | _ ->
                P.newline f;
                statements false cxt f switch_body
            in
            if should_break then (
              P.newline f;
              P.string f L.break;
              semi f);
            cxt))
  in
  cxt

and loop_case_clauses :
    'a. cxt -> P.t -> (P.t -> 'a -> unit) -> ('a * J.case_clause) list -> cxt =
 fun cxt f pp_cond cases ->
  Ext_list.fold_left cases cxt (fun acc x -> pp_one_case_clause acc f pp_cond x)

and vident cxt f (v : J.vident) =
  match v with
  | Id v
  | Qualified ({id = v}, None)
  | Qualified ({id = v; kind = External {default = true}}, _) ->
    Ext_pp_scope.ident cxt f v
  | Qualified ({id; kind = Ml | Runtime}, Some name) ->
    let cxt = Ext_pp_scope.ident cxt f id in
    P.string f L.dot;
    P.string f
      (if name = Js_dump_import_export.default_export then name
       else Ext_ident.convert name);
    cxt
  | Qualified ({id; kind = External _}, Some name) ->
    let cxt = Ext_pp_scope.ident cxt f id in
    Js_dump_property.property_access f name;
    cxt

(* The higher the level, the more likely that inner has to add parens *)
and expression ~level:l cxt f (exp : J.expression) : cxt =
  pp_comment_option f exp.comment;
  expression_desc cxt ~level:l f exp.expression_desc

and expression_desc cxt ~(level : int) f x : cxt =
  match x with
  | Null ->
    P.string f L.null;
    cxt
  | Undefined _ ->
    P.string f L.undefined;
    cxt
  | Var v -> vident cxt f v
  | Bool b ->
    bool f b;
    cxt
  | Seq (e1, e2) ->
    P.cond_paren_group f (level > 0) (fun () ->
        let cxt = expression ~level:0 cxt f e1 in
        comma_sp f;
        expression ~level:0 cxt f e2)
  | Fun {is_method; params; body; env; return_unit; async; directive} ->
    (* TODO: dump for comments *)
    pp_function ?directive ~is_method ~return_unit ~async
      ~fn_state:default_fn_exp_state cxt f params body env
    (* TODO:
       when [e] is [Js_raw_code] with arity
       print it in a more precise way
       It seems the optimizer already did work to make sure
       {[
         Call (Raw_js_code (s, Exp i), el, {Full})
         when Ext_list.length_equal el i
       ]}
    *)
  | Call (e, el, info) ->
    P.cond_paren_group f (level > 15) (fun _ ->
        P.group f 0 (fun _ ->
            match (info, el) with
            | {arity = Full}, _ | _, [] ->
              let cxt =
                P.cond_paren_group f
                  (match e.expression_desc with
                  | Fun _ -> true
                  | _ -> false)
                  (fun () -> expression ~level:15 cxt f e)
              in
              P.paren_group f 0 (fun _ ->
                  match el with
                  | [
                   {
                     expression_desc =
                       Fun
                         {
                           is_method;
                           params;
                           body;
                           env;
                           return_unit;
                           async;
                           directive;
                         };
                   };
                  ] ->
                    pp_function ?directive ~is_method ~return_unit ~async
                      ~fn_state:(No_name {single_arg = true})
                      cxt f params body env
                  | _ ->
                    let el =
                      match el with
                      | [e] when e.expression_desc = Undefined {is_unit = true}
                        ->
                        (* omit passing undefined when the call is f() *)
                        []
                      | _ -> el
                    in
                    arguments cxt f el)
            | _, _ ->
              let len = List.length el in
              if 1 <= len && len <= 8 then (
                Curry_gen.pp_app f len;
                P.paren_group f 0 (fun _ -> arguments cxt f (e :: el)))
              else (
                Curry_gen.pp_app_any f;
                P.paren_group f 0 (fun _ ->
                    arguments cxt f [e; E.array Mutable el]))))
  | FlatCall (e, el) ->
    P.group f 0 (fun _ ->
        let cxt = expression ~level:15 cxt f e in
        P.string f L.dot;
        P.string f L.apply;
        P.paren_group f 1 (fun _ ->
            P.string f L.null;
            comma_sp f;
            expression ~level:1 cxt f el))
  | Tagged_template (call_expr, string_args, value_args) ->
    let cxt = expression cxt ~level f call_expr in
    P.string f "`";
    let rec aux cxt xs ys =
      match (xs, ys) with
      | [], [] -> ()
      | [{J.expression_desc = Str {txt; _}}], [] -> P.string f txt
      | {J.expression_desc = Str {txt; _}} :: x_rest, y :: y_rest ->
        P.string f txt;
        P.string f "${";
        let cxt = expression cxt ~level f y in
        P.string f "}";
        aux cxt x_rest y_rest
      | _ -> assert false
    in
    aux cxt string_args value_args;
    P.string f "`";
    cxt
  | String_index (a, b) ->
    P.group f 1 (fun _ ->
        let cxt = expression ~level:15 cxt f a in
        P.string f L.dot;
        P.string f L.code_point_at;
        (* FIXME: use code_point_at *)
        P.paren_group f 1 (fun _ -> expression ~level:0 cxt f b))
  | Str {delim; txt} ->
    (*TODO --
       when utf8-> it will not escape '\\' which is definitely not we want
    *)
    let () =
      match delim with
      | DStarJ -> P.string f ("\"" ^ txt ^ "\"")
      | DNoQuotes -> P.string f txt
      | DNone -> Js_dump_string.pp_string f txt
    in
    cxt
  | Raw_js_code {code = s; code_info = info} -> (
    match info with
    | Exp exp_info ->
      let raw_paren =
        not
          (match exp_info with
          | Js_literal _ -> true
          | Js_function _ | Js_exp_unknown ->
            false || raw_snippet_exp_simple_enough s)
      in
      if raw_paren then P.string f L.lparen;
      P.string f s;
      if raw_paren then P.string f L.rparen;
      cxt
    | Stmt stmt_info ->
      if stmt_info = Js_stmt_comment then P.string f s
      else (
        P.newline f;
        P.string f s;
        P.newline f);
      cxt)
  | Number v ->
    let s =
      match v with
      | Float {f} -> Js_number.caml_float_literal_to_js_string f
      (* attach string here for float constant folding?*)
      | Int {i; c = Some c} ->
        Format.asprintf "/* %s */%ld" (Ext_util.string_of_int_as_char c) i
      | Int {i; c = None} ->
        Int32.to_string i
        (* check , js convention with ocaml lexical convention *)
      | BigInt {positive; value} ->
        Format.asprintf "%sn" (Bigint_utils.to_string positive value)
    in
    let need_paren =
      if s.[0] = '-' then level > 13
        (* Negative numbers may need to be parenthesized. *)
      else
        level = 15 (* Parenthesize as well when followed by a dot. *)
        && s.[0] <> 'I' (* Infinity *)
        && s.[0] <> 'N' (* NaN *)
    in
    let action _ = P.string f s in
    if need_paren then P.paren f action else action ();
    cxt
  | Is_null_or_undefined e ->
    P.cond_paren_group f (level > 0) (fun _ ->
        let cxt = expression ~level:1 cxt f e in
        P.space f;
        P.string f "==";
        P.space f;
        P.string f L.null;
        cxt)
  | Js_not e ->
    P.cond_paren_group f (level > 13) (fun _ ->
        P.string f "!";
        expression ~level:13 cxt f e)
  | Typeof e ->
    P.string f "typeof";
    P.space f;
    expression ~level:13 cxt f e
  | Bin
      ( Minus,
        {
          expression_desc = Number ((Int {i = 0l; _} | Float {f = "0."}) as desc);
        },
        e )
  (* TODO:
     Handle multiple cases like
     {[ 0. - x ]}
     {[ 0.00 - x ]}
     {[ 0.000 - x ]}
  *)
    ->
    P.cond_paren_group f (level > 13) (fun _ ->
        P.string f
          (match desc with
          | Float _ -> "- "
          | _ -> "-");
        expression ~level:13 cxt f e)
  | Bin
      ( Or,
        {expression_desc = Bin (EqEqEq, e1, {expression_desc = Null})},
        {expression_desc = Bin (EqEqEq, e2, {expression_desc = Undefined _})} )
  | Bin
      ( Or,
        {expression_desc = Bin (EqEqEq, e1, {expression_desc = Undefined _})},
        {expression_desc = Bin (EqEqEq, e2, {expression_desc = Null})} )
    when e1 = e2 ->
    expression_desc cxt ~level:(level : int) f (Is_null_or_undefined e1)
  | Bin (op, e1, e2) ->
    let out, lft, rght = Js_op_util.op_prec op in
    let need_paren =
      level > out
      ||
      match op with
      | Lsl | Lsr | Asr -> true
      | _ -> false
    in
    (* We are more conservative here, to make the generated code more readable
          to the user *)
    P.cond_paren_group f need_paren (fun _ ->
        let cxt = expression ~level:lft cxt f e1 in
        P.space f;
        P.string f (Js_op_util.op_str op);
        P.space f;
        expression ~level:rght cxt f e2)
  | String_append (e1, e2) ->
    let op : Js_op.binop = Plus in
    let out, lft, rght = Js_op_util.op_prec op in
    let need_paren =
      level > out
      ||
      match op with
      | Lsl | Lsr | Asr -> true
      | _ -> false
    in
    P.cond_paren_group f need_paren (fun _ ->
        let cxt = expression ~level:lft cxt f e1 in
        P.space f;
        P.string f "+";
        P.space f;
        expression ~level:rght cxt f e2)
  | Array (el, _) -> (
    (* TODO: simplify for singleton list *)
    match el with
    | [] | [_] -> P.bracket_group f 1 (fun _ -> array_element_list cxt f el)
    | _ -> P.bracket_vgroup f 1 (fun _ -> array_element_list cxt f el))
  | Optional_block (e, identity) ->
    expression ~level cxt f
      (if identity then e
       else E.runtime_call Primitive_modules.option "some" [e])
  | Caml_block (el, _, _, Blk_module fields) ->
    expression_desc cxt ~level f
      (Object
         ( None,
           Ext_list.map_combine fields el (fun x ->
               Js_op.Lit (Ext_ident.convert x)) ))
  (*name convention of Record is slight different from modules*)
  | Caml_block (el, mutable_flag, _, Blk_record {fields}) ->
    if
      Array.length fields <> 0
      && Ext_array.for_alli fields (fun i (v, _) -> string_of_int i = v)
    then expression_desc cxt ~level f (Array (el, mutable_flag))
    else
      let fields =
        Ext_list.array_list_filter_map fields el (fun (f, opt) x ->
            match x.expression_desc with
            | Undefined _ when opt -> None
            | _ -> Some (Js_op.Lit f, x))
      in
      expression_desc cxt ~level f (Object (None, fields))
  | Caml_block (el, _, _, Blk_poly_var _) -> (
    match el with
    | [tag; value] ->
      expression_desc cxt ~level f
        (Object
           ( None,
             [
               (Js_op.Lit Literals.polyvar_hash, tag);
               (Lit Literals.polyvar_value, value);
             ] ))
    | _ -> assert false)
  | Caml_block (el, _, _, ((Blk_extension | Blk_record_ext _) as ext)) ->
    expression_desc cxt ~level f (exn_block_as_obj ~stack:false el ext)
  | Caml_block (el, _, tag, Blk_record_inlined p) ->
    let untagged = Ast_untagged_variants.process_untagged p.attrs in
    let objs =
      let tails =
        Ext_list.combine_array p.fields el (fun (i, opt) -> (Js_op.Lit i, opt))
      in
      let tag_name =
        match Ast_untagged_variants.process_tag_name p.attrs with
        | None -> L.tag
        | Some s -> s
      in
      let tails =
        Ext_list.filter_map tails (fun ((f, optional), x) ->
            match x.expression_desc with
            | Undefined _ when optional -> None
            | _ -> Some (f, x))
      in
      if untagged then tails
      else
        ( Js_op.Lit tag_name,
          (* TAG:xx for inline records *)
          match Ast_untagged_variants.process_tag_type p.attrs with
          | None -> E.str p.name
          | Some t -> E.tag_type t )
        :: tails
    in
    expression_desc cxt ~level f (Object (None, objs))
  | Caml_block (el, _, tag, Blk_constructor p) ->
    let not_is_cons = p.name <> Literals.cons in
    let tag_type = Ast_untagged_variants.process_tag_type p.attrs in
    let untagged = Ast_untagged_variants.process_untagged p.attrs in
    let tag_name =
      match Ast_untagged_variants.process_tag_name p.attrs with
      | None -> L.tag
      | Some s -> s
    in
    let objs =
      let tails =
        Ext_list.mapi_append el
          (fun i e ->
            ( (match (not_is_cons, i) with
              | false, 0 -> Js_op.Lit Literals.hd
              | false, 1 -> Js_op.Lit Literals.tl
              | _ -> Js_op.Lit ("_" ^ string_of_int i)),
              e ))
          (if !Js_config.debug && not_is_cons then [(name_symbol, E.str p.name)]
           else [])
      in
      if untagged || (not_is_cons = false && p.num_nonconst = 1) then tails
      else
        ( Js_op.Lit tag_name,
          (* TAG:xx *)
          match tag_type with
          | None -> E.str p.name
          | Some t -> E.tag_type t )
        :: tails
    in
    let exp =
      match objs with
      | [(_, e)] when untagged -> e.expression_desc
      | _ when untagged -> assert false (* should not happen *)
      (* TODO: put restriction on the variant definitions allowed, to make sure this never happens. *)
      | _ -> J.Object (None, objs)
    in
    expression_desc cxt ~level f exp
  | Caml_block
      ( _,
        _,
        _,
        (Blk_module_export _ | Blk_some | Blk_some_not_nested | Blk_lazy_general)
      ) ->
    assert false
  | Caml_block (el, mutable_flag, _tag, Blk_tuple) ->
    expression_desc cxt ~level f (Array (el, mutable_flag))
  | Caml_block_tag (e, tag) ->
    P.group f 1 (fun _ ->
        let cxt = expression ~level:15 cxt f e in
        P.string f L.dot;
        P.string f tag;
        cxt)
  | Array_index (e, p) ->
    P.cond_paren_group f (level > 15) (fun _ ->
        P.group f 1 (fun _ ->
            let cxt = expression ~level:15 cxt f e in
            P.bracket_group f 1 (fun _ -> expression ~level:0 cxt f p)))
  | Static_index (e, s, _) ->
    P.cond_paren_group f (level > 15) (fun _ ->
        let cxt = expression ~level:15 cxt f e in
        Js_dump_property.property_access f s;
        (* See [ .obj_of_exports]
           maybe in the ast level we should have
           refer and export
        *)
        cxt)
  | Length (e, _) ->
    (*Todo: check parens *)
    P.cond_paren_group f (level > 15) (fun _ ->
        let cxt = expression ~level:15 cxt f e in
        P.string f L.dot;
        P.string f L.length;
        cxt)
  | New (e, el) ->
    P.cond_paren_group f (level > 15) (fun _ ->
        P.group f 0 (fun _ ->
            P.string f L.new_;
            P.space f;
            let cxt = expression ~level:16 cxt f e in
            P.paren_group f 0 (fun _ ->
                match el with
                | Some el -> arguments cxt f el
                | None -> cxt)))
  | Cond (e, e1, e2) ->
    let action () =
      let cxt = expression ~level:3 cxt f e in
      P.space f;
      P.string f L.question;
      P.space f;
      (*
            [level 1] is correct, however
            to make nice indentation , force nested conditional to be parenthesized
          *)
      let cxt = P.group f 1 (fun _ -> expression ~level:3 cxt f e1) in

      P.space f;
      P.string f L.colon_space;
      (* idem *)
      P.group f 1 (fun _ -> expression ~level:3 cxt f e2)
    in
    if level > 2 then P.paren_vgroup f 1 action else action ()
  | Object (dup, lst) ->
    (* #1946 object literal is easy to be
       interpreted as block statement
       here we avoid parens in such case
       {[
         var f = { x : 2 , y : 2}
       ]}
    *)
    P.cond_paren_group f (level > 1) (fun _ ->
        let dup_expression e =
          expression ~level:1 cxt f {e with expression_desc = J.Spread e}
        in
        if lst = [] then
          P.brace f (fun _ ->
              match dup with
              | Some e -> dup_expression e
              | _ -> cxt)
        else
          P.brace_vgroup f 1 (fun _ ->
              let cxt =
                match dup with
                | Some e ->
                  let cxt = dup_expression e in
                  comma_nl f;
                  cxt
                | _ -> cxt
              in
              property_name_and_value_list cxt f lst))
  | Await e ->
    P.cond_paren_group f (level > 13) (fun _ ->
        P.string f "await ";
        expression ~level:13 cxt f e)
  | Spread e ->
    P.cond_paren_group f (level > 13) (fun _ ->
        P.string f "...";
        expression ~level:13 cxt f e)

and property_name_and_value_list cxt f (l : J.property_map) =
  iter_lst cxt f l
    (fun cxt f (pn, e) ->
      match e.expression_desc with
      | Var (Id v | Qualified ({id = v; _}, None)) ->
        let key = Js_dump_property.property_key pn in
        let str, cxt = Ext_pp_scope.str_of_ident cxt v in
        let content =
          (* if key = str then key
             else *)
          key ^ L.colon_space ^ str
        in
        P.string f content;
        cxt
      | _ ->
        let key = Js_dump_property.property_key pn in
        P.string f key;
        P.string f L.colon_space;
        expression ~level:1 cxt f e)
    comma_nl

and array_element_list cxt f (el : E.t list) : cxt =
  iter_lst cxt f el (expression ~level:1) comma_nl

and arguments cxt f (l : E.t list) : cxt =
  iter_lst cxt f l (expression ~level:1) comma_sp

and variable_declaration top cxt f (variable : J.variable_declaration) : cxt =
  (* TODO: print [const/var] for different backends  *)
  match variable with
  | {ident = i; value = None; ident_info; _} ->
    if ident_info.used_stats = Dead_pure then cxt else pp_var_declare cxt f i
  | {ident = name; value = Some e; ident_info = {used_stats; _}} -> (
    match used_stats with
    | Dead_pure -> cxt
    | Dead_non_pure ->
      (* Make sure parens are added correctly *)
      statement_desc top cxt f (J.Exp e)
    | _ -> (
      match e.expression_desc with
      | Fun {is_method; params; body; env; return_unit; async; directive} ->
        pp_function ?directive ~is_method ~return_unit ~async
          ~fn_state:(if top then Name_top name else Name_non_top name)
          cxt f params body env
      | _ ->
        let cxt = pp_var_assign cxt f name in
        let cxt = expression ~level:1 cxt f e in
        semi f;
        cxt))

and ipp_comment : 'a. P.t -> 'a -> unit = fun _f _comment -> ()

(** don't print a new line -- ASI
    FIXME: this still does not work in some cases...
    {[
      return /* ... */
      [... ]
    ]}
*)

and pp_comment f comment =
  if String.length comment > 0 then (
    P.string f "/* ";
    P.string f comment;
    P.string f " */")

and pp_comment_option f comment =
  match comment with
  | None -> ()
  | Some x -> pp_comment f x

and statement top cxt f ({statement_desc = s; comment; _} : J.statement) : cxt =
  pp_comment_option f comment;
  statement_desc top cxt f s

and statement_desc top cxt f (s : J.statement_desc) : cxt =
  match s with
  | Block [] ->
    ipp_comment f L.empty_block;
    (* debugging*)
    cxt
  | Exp {expression_desc = Var _} ->
    (* Does it make sense to optimize here? *)
    (* semi f; *)
    cxt
  | Exp e -> (
    match e.expression_desc with
    | Raw_js_code {code; code_info = Stmt Js_stmt_comment} ->
      P.string f code;
      cxt
    | Raw_js_code {code_info = Exp (Js_literal {comment})} ->
      (match comment with
      (* The %raw is just a comment *)
      | Some s -> P.string f s
      | None -> ());
      cxt
    | Str _ -> cxt
    | _ ->
      let cxt =
        (if exp_need_paren e then P.paren_group f 1 else P.group f 0) (fun _ ->
            expression ~level:0 cxt f e)
      in
      semi f;
      cxt)
  | Block b ->
    (* No braces needed here *)
    ipp_comment f L.start_block;
    let cxt = statements top cxt f b in
    ipp_comment f L.end_block;
    cxt
  | Variable l -> variable_declaration top cxt f l
  | If (e, s1, s2) -> (
    (* TODO: always brace those statements *)
    P.string f L.if_;
    P.space f;
    let cxt = P.paren_group f 1 (fun _ -> expression ~level:0 cxt f e) in
    P.space f;
    let cxt = brace_block cxt f s1 in
    match s2 with
    | [] | [{statement_desc = Block [] | Exp {expression_desc = Var _}}] ->
      P.newline f;
      cxt
    | [({statement_desc = If _} as nest)]
    | [{statement_desc = Block [({statement_desc = If _; _} as nest)]; _}] ->
      P.space f;
      P.string f L.else_;
      P.space f;
      statement false cxt f nest
    | _ :: _ as s2 ->
      P.space f;
      P.string f L.else_;
      P.space f;
      brace_block cxt f s2)
  | While (e, s) ->
    (*  FIXME: print scope as well *)
    let cxt =
      match e.expression_desc with
      | Number (Int {i = 1l}) ->
        P.string f L.while_;
        P.space f;
        P.string f L.lparen;
        P.string f L.true_;
        P.string f L.rparen;
        P.space f;
        cxt
      | _ ->
        P.string f L.while_;
        P.space f;
        let cxt = P.paren_group f 1 (fun _ -> expression ~level:0 cxt f e) in
        P.space f;
        cxt
    in
    let cxt = brace_block cxt f s in
    semi f;
    cxt
  | ForRange (for_ident_expression, finish, id, direction, s) ->
    let action cxt =
      P.vgroup f 0 (fun _ ->
          let cxt =
            P.group f 0 (fun _ ->
                (* The only place that [semi] may have semantics here *)
                P.string f L.for_;
                P.space f;
                let ctx =
                  P.paren_group f 1 (fun _ ->
                      let cxt, new_id =
                        match
                          (for_ident_expression, finish.expression_desc)
                        with
                        | Some ident_expression, (Number _ | Var _) ->
                          let cxt = pp_var_assign cxt f id in
                          (expression ~level:0 cxt f ident_expression, None)
                        | Some ident_expression, _ ->
                          let cxt = pp_var_assign cxt f id in
                          let cxt =
                            expression ~level:1 cxt f ident_expression
                          in
                          comma f;
                          P.space f;
                          let id =
                            Ext_ident.create (Ident.name id ^ "_finish")
                          in
                          let cxt = Ext_pp_scope.ident cxt f id in
                          P.space f;
                          P.string f L.eq;
                          P.space f;
                          (expression ~level:1 cxt f finish, Some id)
                        | None, (Number _ | Var _) -> (cxt, None)
                        | None, _ ->
                          let id =
                            Ext_ident.create (Ident.name id ^ "_finish")
                          in
                          let cxt = pp_var_assign cxt f id in
                          (expression ~level:15 cxt f finish, Some id)
                      in
                      semi f;
                      P.space f;
                      let cxt = Ext_pp_scope.ident cxt f id in
                      P.space f;
                      let right_prec =
                        match direction with
                        | Upto ->
                          let _, _, right = Js_op_util.op_prec Le in
                          P.string f L.le;
                          right
                        | Up ->
                          let _, _, right = Js_op_util.op_prec Lt in
                          P.string f L.lt;
                          right
                        | Downto ->
                          let _, _, right = Js_op_util.op_prec Ge in
                          P.string f L.ge;
                          right
                      in
                      P.space f;
                      let cxt =
                        expression ~level:right_prec cxt f
                          (match new_id with
                          | Some i -> E.var i
                          | None -> finish)
                      in
                      semi f;
                      P.space f;
                      pp_direction f direction;
                      Ext_pp_scope.ident cxt f id)
                in
                P.space f;
                ctx)
          in
          brace_block cxt f s)
    in
    action cxt
  | Continue ->
    continue f;
    cxt
  (* P.newline f;  #2642 *)
  | Debugger ->
    debugger_nl f;
    cxt
  | Break ->
    break_nl f;
    cxt
  | Return e -> (
    match e.expression_desc with
    | Fun {is_method; params; body; env; return_unit; async; directive} ->
      let cxt =
        pp_function ?directive ~return_unit ~is_method ~async
          ~fn_state:Is_return cxt f params body env
      in
      semi f;
      cxt
    | Undefined _ ->
      P.string f L.return;
      semi f;
      cxt
    | _ ->
      return_sp f;
      (* P.string f "return ";(\* ASI -- when there is a comment*\) *)
      P.group f 0 (fun _ ->
          let cxt = expression ~level:0 cxt f e in
          semi f;
          cxt)
      (* There MUST be a space between the return and its
         argument. A line return will not work *)
    )
  | Int_switch (e, cc, def) ->
    P.string f L.switch;
    P.space f;
    let cxt = P.paren_group f 1 (fun _ -> expression ~level:0 cxt f e) in
    P.space f;
    P.brace_vgroup f 1 (fun _ ->
        let cxt =
          loop_case_clauses cxt f (fun f i -> P.string f (string_of_int i)) cc
        in
        match def with
        | None -> cxt
        | Some def ->
          P.newline f;
          P.group f 1 (fun _ ->
              P.string f L.default;
              P.string f L.colon;
              P.newline f;
              statements false cxt f def))
  | String_switch (e, cc, def) ->
    P.string f L.switch;
    P.space f;
    let cxt = P.paren_group f 1 (fun _ -> expression ~level:0 cxt f e) in
    P.space f;
    P.brace_vgroup f 1 (fun _ ->
        let pp_as_value f (tag_type : Ast_untagged_variants.tag_type) =
          let e = E.tag_type tag_type in
          ignore @@ expression_desc cxt ~level:0 f e.expression_desc
        in
        let cxt = loop_case_clauses cxt f pp_as_value cc in
        match def with
        | None -> cxt
        | Some def ->
          P.newline f;
          P.group f 1 (fun _ ->
              P.string f L.default;
              P.string f L.colon;
              P.newline f;
              statements false cxt f def))
  | Throw e ->
    let e =
      match e.expression_desc with
      | Caml_block (el, _, _, ((Blk_extension | Blk_record_ext _) as ext)) ->
        {e with expression_desc = exn_block_as_obj ~stack:true el ext}
      | _ -> e
    in
    P.string f L.throw;
    P.space f;
    P.group f 0 (fun _ ->
        let cxt = expression ~level:0 cxt f e in
        semi f;
        cxt)
  (* There must be a space between the return and its
     argument. A line return would not work *)
  | Try (b, ctch, fin) ->
    P.vgroup f 0 (fun _ ->
        P.string f L.try_;
        P.space f;
        let cxt = brace_block cxt f b in
        let cxt =
          match ctch with
          | None -> cxt
          | Some (i, b) ->
            P.string f " catch (";
            let cxt = Ext_pp_scope.ident cxt f i in
            P.string f ") ";
            brace_block cxt f b
        in
        match fin with
        | None -> cxt
        | Some b ->
          P.group f 1 (fun _ ->
              P.string f L.finally;
              P.space f;
              brace_block cxt f b))

and function_body ?directive (cxt : cxt) f ~return_unit (b : J.block) : unit =
  (match directive with
  | None -> ()
  | Some directive ->
    P.newline f;
    P.string f directive;
    P.string f ";";
    P.newline f);
  match b with
  | [] -> ()
  | [s] -> (
    match s.statement_desc with
    | If
        ( bool,
          then_,
          [{statement_desc = Return {expression_desc = Undefined _}}] ) ->
      ignore
        (statement false cxt f {s with statement_desc = If (bool, then_, [])}
          : cxt)
    | Return {expression_desc = Undefined _} -> ()
    | Return exp when return_unit ->
      ignore (statement false cxt f (S.exp exp) : cxt)
    | _ -> ignore (statement false cxt f s : cxt))
  | [s; {statement_desc = Return {expression_desc = Undefined _}}] ->
    ignore (statement false cxt f s : cxt)
  | s :: r ->
    let cxt = statement false cxt f s in
    P.newline f;
    function_body cxt f r ~return_unit

and brace_block cxt f b =
  (* This one is for '{' *)
  P.brace_vgroup f 1 (fun _ -> statements false cxt f b)

(* main entry point *)
and statements top cxt f b =
  iter_lst cxt f b
    (fun cxt f s -> statement top cxt f s)
    (if top then P.at_least_two_lines else P.newline)

let string_of_block (block : J.block) =
  let buffer = Buffer.create 50 in
  let f = P.from_buffer buffer in
  let (_ : cxt) = statements true Ext_pp_scope.empty f block in
  P.flush f ();
  Buffer.contents buffer

let string_of_expression (e : J.expression) =
  let buffer = Buffer.create 50 in
  let f = P.from_buffer buffer in
  let (_ : cxt) = expression ~level:0 Ext_pp_scope.empty f e in
  P.flush f ();
  Buffer.contents buffer

let () = E.string_of_expression := string_of_expression
