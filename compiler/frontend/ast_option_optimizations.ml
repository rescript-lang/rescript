open Parsetree
open Longident

(*
   Optimise calls to Option.forEach/map/flatMap so they produce the same switch
   structure as handwritten code. We only rewrite calls whose callback is a
   simple literal lambda or identifier; more complex callbacks are left intact
   to preserve ReScript's call-by-value semantics.
*)

let value_name = "__res_option_value"

type option_call = ForEach | Map | FlatMap

(* Inlineable callbacks are bare identifiers (possibly wrapped in coercions or
   type annotations). Those can be applied directly inside the emitted switch
   without introducing a let-binding that might change evaluation behaviour. *)
let rec callback_is_inlineable expr =
  match expr.pexp_desc with
  | Pexp_ident _ -> true
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
    callback_is_inlineable inner
  | _ -> false

(* Detect literal lambdas (ignoring type annotations) so we can reuse their
   argument binder in the rewritten switch. *)
let rec inline_lambda expr =
  match expr.pexp_desc with
  | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) ->
    inline_lambda inner
  | Pexp_fun {arg_label = Asttypes.Nolabel; lhs; rhs; async = false} ->
    Some (lhs, rhs)
  | _ -> None

let transform (expr : Parsetree.expression) : Parsetree.expression =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident
                {txt = Ldot (Lident ("Option" | "Stdlib_Option"), fname)};
          };
        args = [(_, opt_expr); (_, func_expr)];
      } -> (
    let call_kind =
      match fname with
      | "forEach" -> Some ForEach
      | "map" -> Some Map
      | "flatMap" -> Some FlatMap
      | _ -> None
    in
    match call_kind with
    | None -> expr
    | Some call_kind -> (
      let loc_ghost = {expr.pexp_loc with loc_ghost = true} in
      let emit_option_match value_pat result_expr =
        let some_rhs =
          match call_kind with
          | ForEach | FlatMap -> result_expr
          | Map ->
            Ast_helper.Exp.construct ~loc:loc_ghost
              {txt = Lident "Some"; loc = loc_ghost}
              (Some result_expr)
        in
        let none_rhs =
          match call_kind with
          | ForEach ->
            Ast_helper.Exp.construct ~loc:loc_ghost
              {txt = Lident "()"; loc = loc_ghost}
              None
          | Map | FlatMap ->
            Ast_helper.Exp.construct ~loc:loc_ghost
              {txt = Lident "None"; loc = loc_ghost}
              None
        in
        let mk_case ctor payload rhs =
          {
            Parsetree.pc_bar = None;
            pc_lhs =
              Ast_helper.Pat.construct ~loc:loc_ghost
                {txt = Lident ctor; loc = loc_ghost}
                payload;
            pc_guard = None;
            pc_rhs = rhs;
          }
        in
        let some_case = mk_case "Some" (Some value_pat) some_rhs in
        let none_case = mk_case "None" None none_rhs in
        let transformed =
          Ast_helper.Exp.match_ ~loc:loc_ghost opt_expr [some_case; none_case]
        in
        {
          transformed with
          pexp_loc = expr.pexp_loc;
          pexp_attributes = expr.pexp_attributes;
        }
      in
      match inline_lambda func_expr with
      (* Literal lambda with a simple binder: reuse the binder directly inside
         the generated switch, so the body runs exactly once with the option's
         payload. *)
      | Some ({ppat_desc = Parsetree.Ppat_var {txt}}, body) ->
        let value_pat =
          Ast_helper.Pat.var ~loc:loc_ghost {txt; loc = loc_ghost}
        in
        emit_option_match value_pat body
      (* Callback is a simple identifier (possibly annotated). Apply it inside
         the switch so evaluation order matches handwritten code. *)
      | _ when callback_is_inlineable func_expr ->
        let value_pat =
          Ast_helper.Pat.var ~loc:loc_ghost {txt = value_name; loc = loc_ghost}
        in
        let value_ident =
          Ast_helper.Exp.ident ~loc:loc_ghost
            {txt = Lident value_name; loc = loc_ghost}
        in
        let apply_callback =
          Ast_helper.Exp.apply ~loc:loc_ghost func_expr
            [(Asttypes.Nolabel, value_ident)]
        in
        emit_option_match value_pat apply_callback
      (* Complex callbacks are left as-is so we don't change when they run. *)
      | _ -> expr))
  | _ -> expr
