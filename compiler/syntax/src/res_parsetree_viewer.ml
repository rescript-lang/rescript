open Parsetree

let arrow_type ?(max_arity = max_int) ct =
  let has_as_attr attrs =
    Ext_list.exists attrs (fun (x, _) -> x.Asttypes.txt = "as")
  in
  let rec process attrs_before acc typ arity =
    match typ with
    | _ when arity < 0 -> (attrs_before, List.rev acc, typ)
    | {ptyp_desc = Ptyp_arrow {arity = Some _}; ptyp_attributes = []}
      when acc <> [] ->
      (attrs_before, List.rev acc, typ)
    | {
     ptyp_desc = Ptyp_arrow {lbl = Nolabel as lbl; arg; ret};
     ptyp_attributes = [];
    } ->
      let arg = ([], lbl, arg) in
      process attrs_before (arg :: acc) ret (arity - 1)
    | {
     ptyp_desc = Ptyp_arrow {lbl = Nolabel};
     ptyp_attributes = [({txt = "bs"}, _)];
    } ->
      (* stop here, the uncurried attribute always indicates the beginning of an arrow function
       * e.g. `(. int) => (. int)` instead of `(. int, . int)` *)
      (attrs_before, List.rev acc, typ)
    | {ptyp_desc = Ptyp_arrow {lbl = Nolabel}; ptyp_attributes = _attrs} as
      return_type ->
      let args = List.rev acc in
      (attrs_before, args, return_type)
    | {
     ptyp_desc = Ptyp_arrow {lbl = (Labelled _ | Optional _) as lbl; arg; ret};
     ptyp_attributes = attrs;
    } ->
      (* Res_core.parse_es6_arrow_type has a workaround that removed an extra arity for the function if the
         argument is a Ptyp_any with @as attribute i.e. ~x: @as(`{prop: value}`) _.

         When this case is encountered we add that missing arity so the arrow is printed properly.
      *)
      let arity =
        match arg with
        | {ptyp_desc = Ptyp_any; ptyp_attributes = attrs1}
          when has_as_attr attrs1 ->
          arity
        | _ -> arity - 1
      in
      let arg = (attrs, lbl, arg) in
      process attrs_before (arg :: acc) ret arity
    | typ -> (attrs_before, List.rev acc, typ)
  in
  match ct with
  | {ptyp_desc = Ptyp_arrow {lbl = Nolabel}; ptyp_attributes = attrs1} as typ ->
    process attrs1 [] {typ with ptyp_attributes = []} max_arity
  | typ -> process [] [] typ max_arity

let functor_type modtype =
  let rec process acc modtype =
    match modtype with
    | {
     pmty_desc = Pmty_functor (lbl, arg_type, return_type);
     pmty_attributes = attrs;
    } ->
      let arg = (attrs, lbl, arg_type) in
      process (arg :: acc) return_type
    | mod_type -> (List.rev acc, mod_type)
  in
  process [] modtype

let has_await_attribute attrs =
  List.exists
    (function
      | {Location.txt = "res.await"}, _ -> true
      | _ -> false)
    attrs

let has_inline_record_definition_attribute attrs =
  List.exists
    (function
      | {Location.txt = "res.inlineRecordDefinition"}, _ -> true
      | _ -> false)
    attrs

let has_res_pat_variant_spread_attribute attrs =
  List.exists
    (function
      | {Location.txt = "res.patVariantSpread"}, _ -> true
      | _ -> false)
    attrs

let has_dict_pattern_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) ->
         txt = "res.dictPattern")
  |> Option.is_some

let collect_array_expressions expr =
  match expr.pexp_desc with
  | Pexp_array exprs -> (exprs, None)
  | _ -> ([], Some expr)

let collect_list_expressions expr =
  let rec collect acc expr =
    match expr.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) -> (List.rev acc, None)
    | Pexp_construct
        ( {txt = Longident.Lident "::"},
          Some {pexp_desc = Pexp_tuple (hd :: [tail])} ) ->
      collect (hd :: acc) tail
    | _ -> (List.rev acc, Some expr)
  in
  collect [] expr

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
let rewrite_underscore_apply expr =
  let expr_fun =
    if Ast_uncurried.expr_is_uncurried_fun expr then
      Ast_uncurried.expr_extract_uncurried_fun expr
    else expr
  in
  match expr_fun.pexp_desc with
  | Pexp_fun
      {
        arg_label = Nolabel;
        default = None;
        lhs = {ppat_desc = Ppat_var {txt = "__x"}};
        rhs = {pexp_desc = Pexp_apply {funct = call_expr; args}} as e;
      } ->
    let new_args =
      List.map
        (fun arg ->
          match arg with
          | ( lbl,
              ({pexp_desc = Pexp_ident ({txt = Longident.Lident "__x"} as lid)}
               as arg_expr) ) ->
            ( lbl,
              {
                arg_expr with
                pexp_desc = Pexp_ident {lid with txt = Longident.Lident "_"};
              } )
          | arg -> arg)
        args
    in
    {
      e with
      pexp_desc =
        Pexp_apply {funct = call_expr; args = new_args; partial = false};
    }
  | _ -> expr

type fun_param_kind =
  | Parameter of {
      attrs: Parsetree.attributes;
      lbl: Asttypes.arg_label;
      default_expr: Parsetree.expression option;
      pat: Parsetree.pattern;
    }
  | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

let fun_expr expr_ =
  let async = Ast_async.dig_async_payload_from_function expr_ in
  let rec collect_params ~n_fun ~params expr =
    match expr with
    | {
     pexp_desc =
       Pexp_fun
         {
           arg_label = lbl;
           default = default_expr;
           lhs = pattern;
           rhs = return_expr;
           arity;
         };
     pexp_attributes = attrs;
    }
      when arity = None || n_fun = 0 ->
      let parameter = Parameter {attrs; lbl; default_expr; pat = pattern} in
      collect_params ~n_fun:(n_fun + 1) ~params:(parameter :: params)
        return_expr
    | _ -> (async, List.rev params, expr)
  in
  (* Turns (type t, type u, type z) into "type t u z" *)
  let rec collect_new_types acc return_expr =
    match return_expr with
    | {pexp_desc = Pexp_newtype (string_loc, return_expr)} ->
      collect_new_types (string_loc :: acc) return_expr
    | return_expr -> (List.rev acc, return_expr)
  in
  match expr_ with
  | {pexp_desc = Pexp_newtype (string_loc, rest)} ->
    let string_locs, return_expr = collect_new_types [string_loc] rest in
    let param = NewTypes {attrs = []; locs = string_locs} in
    collect_params ~n_fun:0 ~params:[param] return_expr
  | _ -> collect_params ~n_fun:0 ~params:[] {expr_ with pexp_attributes = []}

let process_braces_attr expr =
  match expr.pexp_attributes with
  | (({txt = "res.braces" | "ns.braces"}, _) as attr) :: attrs ->
    (Some attr, {expr with pexp_attributes = attrs})
  | _ -> (None, expr)

let filter_parsing_attrs attrs =
  List.filter
    (fun attr ->
      match attr with
      | ( {
            Location.txt =
              ( "res.braces" | "ns.braces" | "res.iflet" | "res.ternary"
              | "res.await" | "res.template" | "res.taggedTemplate"
              | "res.patVariantSpread" | "res.dictPattern"
              | "res.inlineRecordDefinition" );
          },
          _ ) ->
        false
      | _ -> true)
    attrs

let is_block_expr expr =
  match expr.pexp_desc with
  | Pexp_letmodule _ | Pexp_letexception _ | Pexp_let _ | Pexp_open _
  | Pexp_sequence _ ->
    true
  | _ -> false

let is_braced_expr expr =
  match process_braces_attr expr with
  | Some _, _ -> true
  | _ -> false

let is_multiline_text txt =
  let len = String.length txt in
  let rec check i =
    if i >= len then false
    else
      let c = String.unsafe_get txt i in
      match c with
      | '\010' | '\013' -> true
      | '\\' -> if i + 2 = len then false else check (i + 2)
      | _ -> check (i + 1)
  in
  check 0

let is_huggable_expression expr =
  match expr.pexp_desc with
  | Pexp_array _ | Pexp_tuple _
  | Pexp_constant (Pconst_string (_, Some _))
  | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
  | Pexp_extension ({txt = "obj"}, _)
  | Pexp_record _ ->
    true
  | _ when is_block_expr expr -> true
  | _ when is_braced_expr expr -> true
  | Pexp_constant (Pconst_string (txt, None)) when is_multiline_text txt -> true
  | _ -> false

let is_huggable_rhs expr =
  match expr.pexp_desc with
  | Pexp_array _ | Pexp_tuple _
  | Pexp_extension ({txt = "obj"}, _)
  | Pexp_record _ ->
    true
  | _ when is_braced_expr expr -> true
  | _ -> false

let is_huggable_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_array _ | Ppat_tuple _ | Ppat_record _ | Ppat_variant _
  | Ppat_construct _ ->
    true
  | _ -> false

let operator_precedence operator =
  match operator with
  | ":=" -> 1
  | "||" -> 2
  | "&&" -> 3
  | "==" | "===" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" -> 4
  | "+" | "+." | "-" | "-." | "++" -> 5
  | "*" | "*." | "/" | "/." | "%" -> 6
  | "**" -> 7
  | "#" | "##" | "->" -> 8
  | _ -> 0

let is_unary_operator operator =
  match operator with
  | "~+" | "~+." | "~-" | "~-." | "not" -> true
  | _ -> false

let is_unary_expression expr =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}};
        args = [(Nolabel, _arg)];
      }
    when is_unary_operator operator ->
    true
  | _ -> false

(* TODO: tweak this to check for ghost ^ as template literal *)
let is_binary_operator operator =
  match operator with
  | ":=" | "||" | "&&" | "==" | "===" | "<" | ">" | "!=" | "!==" | "<=" | ">="
  | "|>" | "+" | "+." | "-" | "-." | "++" | "*" | "*." | "/" | "/." | "**"
  | "->" | "<>" | "%" ->
    true
  | _ -> false

let is_binary_expression expr =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident {txt = Longident.Lident operator; loc = operator_loc};
          };
        args = [(Nolabel, _operand1); (Nolabel, _operand2)];
      }
    when is_binary_operator operator
         && not (operator_loc.loc_ghost && operator = "++")
         (* template literal *) ->
    true
  | _ -> false

let is_equality_operator operator =
  match operator with
  | "==" | "===" | "!=" | "!==" -> true
  | _ -> false

let is_rhs_binary_operator operator =
  match operator with
  | "**" -> true
  | _ -> false

let flattenable_operators parent_operator child_operator =
  let prec_parent = operator_precedence parent_operator in
  let prec_child = operator_precedence child_operator in
  if prec_parent == prec_child then
    not
      (is_equality_operator parent_operator
      && is_equality_operator child_operator)
  else false

let rec has_if_let_attribute attrs =
  match attrs with
  | [] -> false
  | ({Location.txt = "res.iflet"}, _) :: _ -> true
  | _ :: attrs -> has_if_let_attribute attrs

let is_if_let_expr expr =
  match expr with
  | {pexp_attributes = attrs; pexp_desc = Pexp_match _}
    when has_if_let_attribute attrs ->
    true
  | _ -> false

let has_attributes attrs =
  List.exists
    (fun attr ->
      match attr with
      | ( {
            Location.txt =
              ( "res.braces" | "ns.braces" | "res.iflet" | "res.ternary"
              | "res.await" | "res.template" | "res.inlineRecordDefinition" );
          },
          _ ) ->
        false
      (* Remove the fragile pattern warning for iflet expressions *)
      | ( {Location.txt = "warning"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string ("-4", None))}, _);
              };
            ] ) ->
        not (has_if_let_attribute attrs)
      | _ -> true)
    attrs

let is_array_access expr =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident
                {txt = Longident.Ldot (Longident.Lident "Array", "get")};
          };
        args = [(Nolabel, _parentExpr); (Nolabel, _memberExpr)];
      } ->
    true
  | _ -> false

type if_condition_kind =
  | If of Parsetree.expression
  | IfLet of Parsetree.pattern * Parsetree.expression

let collect_if_expressions expr =
  let rec collect acc expr =
    let expr_loc = expr.pexp_loc in
    match expr.pexp_desc with
    | Pexp_ifthenelse (if_expr, then_expr, Some else_expr) ->
      collect ((expr_loc, If if_expr, then_expr) :: acc) else_expr
    | Pexp_ifthenelse (if_expr, then_expr, (None as else_expr)) ->
      let ifs = List.rev ((expr_loc, If if_expr, then_expr) :: acc) in
      (ifs, else_expr)
    | Pexp_match
        ( condition,
          [
            {pc_lhs = pattern; pc_guard = None; pc_rhs = then_expr};
            {
              pc_rhs =
                {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)};
            };
          ] )
      when is_if_let_expr expr ->
      let ifs =
        List.rev ((expr_loc, IfLet (pattern, condition), then_expr) :: acc)
      in
      (ifs, None)
    | Pexp_match
        ( condition,
          [
            {pc_lhs = pattern; pc_guard = None; pc_rhs = then_expr};
            {pc_rhs = else_expr};
          ] )
      when is_if_let_expr expr ->
      collect
        ((expr_loc, IfLet (pattern, condition), then_expr) :: acc)
        else_expr
    | _ -> (List.rev acc, Some expr)
  in
  collect [] expr

let rec has_ternary_attribute attrs =
  match attrs with
  | [] -> false
  | ({Location.txt = "res.ternary"}, _) :: _ -> true
  | _ :: attrs -> has_ternary_attribute attrs

let is_ternary_expr expr =
  match expr with
  | {pexp_attributes = attrs; pexp_desc = Pexp_ifthenelse _}
    when has_ternary_attribute attrs ->
    true
  | _ -> false

let collect_ternary_parts expr =
  let rec collect acc expr =
    match expr with
    | {
     pexp_attributes = attrs;
     pexp_desc = Pexp_ifthenelse (condition, consequent, Some alternate);
    }
      when has_ternary_attribute attrs ->
      collect ((condition, consequent) :: acc) alternate
    | alternate -> (List.rev acc, alternate)
  in
  collect [] expr

let parameters_should_hug parameters =
  match parameters with
  | [Parameter {attrs = []; lbl = Nolabel; default_expr = None; pat}]
    when is_huggable_pattern pat ->
    true
  | _ -> false

let filter_ternary_attributes attrs =
  List.filter
    (fun attr ->
      match attr with
      | {Location.txt = "res.ternary"}, _ -> false
      | _ -> true)
    attrs

let filter_fragile_match_attributes attrs =
  List.filter
    (fun attr ->
      match attr with
      | ( {Location.txt = "warning"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string ("-4", _))}, _);
              };
            ] ) ->
        false
      | _ -> true)
    attrs

let is_jsx_expression expr =
  let rec loop attrs =
    match attrs with
    | [] -> false
    | ({Location.txt = "JSX"}, _) :: _ -> true
    | _ :: attrs -> loop attrs
  in
  match expr.pexp_desc with
  | Pexp_apply _ -> loop expr.Parsetree.pexp_attributes
  | _ -> false

let has_jsx_attribute attributes =
  let rec loop attrs =
    match attrs with
    | [] -> false
    | ({Location.txt = "JSX"}, _) :: _ -> true
    | _ :: attrs -> loop attrs
  in
  loop attributes

let should_indent_binary_expr expr =
  let same_precedence_sub_expression operator sub_expression =
    match sub_expression with
    | {
     pexp_desc =
       Pexp_apply
         {
           funct = {pexp_desc = Pexp_ident {txt = Longident.Lident sub_operator}};
           args = [(Nolabel, _lhs); (Nolabel, _rhs)];
         };
    }
      when is_binary_operator sub_operator ->
      flattenable_operators operator sub_operator
    | _ -> true
  in
  match expr with
  | {
   pexp_desc =
     Pexp_apply
       {
         funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}};
         args = [(Nolabel, lhs); (Nolabel, _rhs)];
       };
  }
    when is_binary_operator operator ->
    is_equality_operator operator
    || (not (same_precedence_sub_expression operator lhs))
    || operator = ":="
  | _ -> false

let should_inline_rhs_binary_expr rhs =
  match rhs.pexp_desc with
  | Parsetree.Pexp_constant _ | Pexp_let _ | Pexp_letmodule _
  | Pexp_letexception _ | Pexp_sequence _ | Pexp_open _ | Pexp_ifthenelse _
  | Pexp_for _ | Pexp_while _ | Pexp_try _ | Pexp_array _ | Pexp_record _ ->
    true
  | _ -> false

let is_printable_attribute attr =
  match attr with
  | ( {
        Location.txt =
          ( "res.iflet" | "res.braces" | "ns.braces" | "JSX" | "res.await"
          | "res.template" | "res.ternary" | "res.inlineRecordDefinition" );
      },
      _ ) ->
    false
  | _ -> true

let has_printable_attributes attrs = List.exists is_printable_attribute attrs

let filter_printable_attributes attrs = List.filter is_printable_attribute attrs

let partition_printable_attributes attrs =
  List.partition is_printable_attribute attrs

let is_fun_newtype expr =
  match expr.pexp_desc with
  | Pexp_fun _ | Pexp_newtype _ -> true
  | _ -> Ast_uncurried.expr_is_uncurried_fun expr

let requires_special_callback_printing_last_arg args =
  let rec loop args =
    match args with
    | [] -> false
    | [(_, expr)] when is_fun_newtype expr -> true
    | (_, expr) :: _ when is_fun_newtype expr -> false
    | _ :: rest -> loop rest
  in
  loop args

let requires_special_callback_printing_first_arg args =
  let rec loop args =
    match args with
    | [] -> true
    | (_, expr) :: _ when is_fun_newtype expr -> false
    | _ :: rest -> loop rest
  in
  match args with
  | [(_, expr)] when is_fun_newtype expr -> false
  | (_, expr) :: rest when is_fun_newtype expr -> loop rest
  | _ -> false

let mod_expr_apply mod_expr =
  let rec loop acc mod_expr =
    match mod_expr with
    | {pmod_desc = Pmod_apply (next, arg)} -> loop (arg :: acc) next
    | _ -> (acc, mod_expr)
  in
  loop [] mod_expr

let mod_expr_functor mod_expr =
  let rec loop acc mod_expr =
    match mod_expr with
    | {
     pmod_desc = Pmod_functor (lbl, mod_type, return_mod_expr);
     pmod_attributes = attrs;
    } ->
      let param = (attrs, lbl, mod_type) in
      loop (param :: acc) return_mod_expr
    | return_mod_expr -> (List.rev acc, return_mod_expr)
  in
  loop [] mod_expr

let rec collect_patterns_from_list_construct acc pattern =
  let open Parsetree in
  match pattern.ppat_desc with
  | Ppat_construct
      ({txt = Longident.Lident "::"}, Some {ppat_desc = Ppat_tuple [pat; rest]})
    ->
    collect_patterns_from_list_construct (pat :: acc) rest
  | _ -> (List.rev acc, pattern)

let has_template_literal_attr attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Location.txt = "res.template"}, _ -> true
      | _ -> false)
    attrs

let has_tagged_template_literal_attr attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Location.txt = "res.taggedTemplate"}, _ -> true
      | _ -> false)
    attrs

let is_template_literal expr =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "++"}};
        args = [(Nolabel, _); (Nolabel, _)];
      }
    when has_template_literal_attr expr.pexp_attributes ->
    true
  | Pexp_constant (Pconst_string (_, Some "")) -> true
  | Pexp_constant _ when has_template_literal_attr expr.pexp_attributes -> true
  | _ -> false

let is_tagged_template_literal expr =
  match expr with
  | {pexp_desc = Pexp_apply _; pexp_attributes = attrs} ->
    has_tagged_template_literal_attr attrs
  | _ -> false

let has_spread_attr attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Location.txt = "res.spread"}, _ -> true
      | _ -> false)
    attrs

let is_spread_belt_list_concat expr =
  match expr.pexp_desc with
  | Pexp_ident
      {
        txt =
          Longident.Ldot
            (Longident.Ldot (Longident.Lident "Belt", "List"), "concatMany");
      } ->
    has_spread_attr expr.pexp_attributes
  | _ -> false

let is_spread_belt_array_concat expr =
  match expr.pexp_desc with
  | Pexp_ident
      {
        txt =
          Longident.Ldot
            (Longident.Ldot (Longident.Lident "Belt", "Array"), "concatMany");
      } ->
    has_spread_attr expr.pexp_attributes
  | _ -> false

(* Blue | Red | Green -> [Blue; Red; Green] *)
let collect_or_pattern_chain pat =
  let rec loop pattern chain =
    match pattern.ppat_desc with
    | Ppat_or (left, right) -> loop left (right :: chain)
    | _ -> pattern :: chain
  in
  loop pat []

let is_single_pipe_expr expr =
  (* handles:
   *   x
   *   ->Js.Dict.get("wm-property")
   *   ->Option.flatMap(Js.Json.decodeString)
   *   ->Option.flatMap(x =>
   *     switch x {
   *     | "like-of" => Some(#like)
   *     | "repost-of" => Some(#repost)
   *     | _ => None
   *     }
   *   )
   *)
  let is_pipe_expr expr =
    match expr.pexp_desc with
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Longident.Lident ("->" | "|>")}};
          args = [(Nolabel, _operand1); (Nolabel, _operand2)];
        } ->
      true
    | _ -> false
  in
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident ("->" | "|>")}};
        args = [(Nolabel, operand1); (Nolabel, _operand2)];
      }
    when not (is_pipe_expr operand1) ->
    true
  | _ -> false

let is_underscore_apply_sugar expr =
  match expr.pexp_desc with
  | Pexp_fun
      {
        arg_label = Nolabel;
        default = None;
        lhs = {ppat_desc = Ppat_var {txt = "__x"}};
        rhs = {pexp_desc = Pexp_apply _};
      } ->
    true
  | _ -> false

let is_rewritten_underscore_apply_sugar expr =
  match expr.pexp_desc with
  | Pexp_ident {txt = Longident.Lident "_"} -> true
  | _ -> false

let is_tuple_array (expr : Parsetree.expression) =
  let is_plain_tuple (expr : Parsetree.expression) =
    match expr with
    | {pexp_desc = Pexp_tuple _} -> true
    | _ -> false
  in
  match expr with
  | {pexp_desc = Pexp_array items} -> List.for_all is_plain_tuple items
  | _ -> false
