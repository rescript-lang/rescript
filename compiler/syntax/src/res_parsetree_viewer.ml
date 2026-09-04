open Parsetree

let arrow_type ct =
  match ct with
  | {ptyp_desc = Ptyp_arrow {params; ret}; ptyp_attributes = attrs} ->
    (attrs, params, ret)
  | typ -> ([], [], typ)

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

let expr_is_await e =
  match e.pexp_desc with
  | Pexp_await _ -> true
  | _ -> false

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

let has_dict_spread_attribute attrs =
  attrs
  |> List.find_opt (fun (({txt}, _) : Parsetree.attribute) ->
      txt = "res.dictSpread")
  |> Option.is_some

type dict_expr_part =
  | DictExprRows of Parsetree.expression
  | DictExprSpread of Parsetree.expression

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
  match expr.pexp_desc with
  | Pexp_fun
      {
        params =
          [
            {
              p_lbl = Nolabel;
              p_default = None;
              p_pat = {ppat_desc = Ppat_var {txt = "__x"}};
            };
          ];
        body = {pexp_desc = Pexp_apply {funct = call_expr; args}} as e;
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
        Pexp_apply
          {
            funct = call_expr;
            args = new_args;
            partial = false;
            transformed_jsx = false;
          };
    }
  | _ -> expr

(* For pipe RHS: (__x) => f(__x, a, b) -----> f(a, b)
   Note: Ppat_var "__x" and Pexp_ident "__x" represent `_` placeholders in user code.
   Omits the first __x argument only if it's the sole occurrence.
   If multiple __x exist (e.g., f(__x, __x, b)), keeps all to preserve semantics. *)
let rewrite_underscore_apply_in_pipe expr =
  let is_underscore_arg = function
    | _, {pexp_desc = Pexp_ident {txt = Longident.Lident "__x"}} -> true
    | _ -> false
  in
  let convert_underscore_to_placeholder arg =
    match arg with
    | ( lbl,
        ({pexp_desc = Pexp_ident ({txt = Longident.Lident "__x"} as lid)} as
         arg_expr) ) ->
      ( lbl,
        {
          arg_expr with
          pexp_desc = Pexp_ident {lid with txt = Longident.Lident "_"};
        } )
    | arg -> arg
  in
  match expr.pexp_desc with
  | Pexp_fun
      {
        params =
          [
            {
              p_lbl = Nolabel;
              p_default = None;
              p_pat = {ppat_desc = Ppat_var {txt = "__x"}};
            };
          ];
        body = {pexp_desc = Pexp_apply {funct; args}} as e;
      } -> (
    match args with
    | first_arg :: rest_args when is_underscore_arg first_arg ->
      if List.exists is_underscore_arg rest_args then
        (* Multiple __x - keep all to preserve semantics *)
        rewrite_underscore_apply expr
      else
        (* Single __x in first position - safe to omit *)
        {
          e with
          pexp_desc =
            Pexp_apply
              {
                funct;
                args = List.map convert_underscore_to_placeholder rest_args;
                partial = false;
                transformed_jsx = false;
              };
        }
    | _ -> rewrite_underscore_apply expr)
  | _ -> expr

type fun_param_kind =
  | Parameter of {
      attrs: Parsetree.attributes;
      lbl: Asttypes.arg_label;
      default_expr: Parsetree.expression option;
      pat: Parsetree.pattern;
    }
  | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

(* Turns the function's newtypes into printable groups: a new group starts
   at each attribute-bearing newtype, matching how the parser distributes
   group attributes. *)
let group_newtypes newtypes =
  List.fold_left
    (fun groups ((name : string Asttypes.loc), attrs) ->
      match groups with
      | (gattrs, locs) :: rest when attrs = [] ->
        (gattrs, locs @ [name]) :: rest
      | _ -> (attrs, [name]) :: groups)
    [] newtypes
  |> List.rev

let fun_expr expr_ =
  let params_of_fun params =
    List.map
      (fun {p_attrs; p_lbl; p_default; p_pat} ->
        Parameter
          {attrs = p_attrs; lbl = p_lbl; default_expr = p_default; pat = p_pat})
      params
  in
  let newtype_params newtypes =
    group_newtypes newtypes
    |> List.map (fun (attrs, locs) -> NewTypes {attrs; locs})
  in
  match expr_ with
  | {pexp_desc = Pexp_fun {newtypes; params; body; async}} ->
    (async, newtype_params newtypes @ params_of_fun params, body)
  | _ -> (false, [], expr_)

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
              | "res.await" | "res.patVariantSpread" | "res.dictPattern"
              | "res.dictSpread" | "res.inlineRecordDefinition" );
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
  | Pexp_constant (Pconst_json _ | Pconst_char _)
  | Pexp_template {values = []}
  | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
  | Pexp_object_literal _ | Pexp_record _ ->
    true
  | _ when is_block_expr expr -> true
  | _ when is_braced_expr expr -> true
  | Pexp_constant (Pconst_string payload)
    when is_multiline_text (String_literal.string_source payload) ->
    true
  | Pexp_constant (Pconst_raw_source source) when is_multiline_text source ->
    true
  | _ -> false

let is_huggable_rhs expr =
  match expr.pexp_desc with
  | Pexp_array _ | Pexp_tuple _ | Pexp_object_literal _ | Pexp_record _ -> true
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
  | "|||" -> 4
  | "^^^" -> 5
  | "&&&" -> 6
  | "==" | "===" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" -> 7
  | "<<" | ">>" | ">>>" -> 8
  | "+" | "+." | "-" | "-." | "++" -> 9
  | "*" | "*." | "/" | "/." | "%" -> 10
  | "**" -> 11
  | "#" | "##" | "->" -> 12
  | _ -> 0

let is_unary_operator operator =
  match operator with
  | "~+" | "~+." | "~-" | "~-." | "~~~" | "not" -> true
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

let is_binary_operator operator =
  match operator with
  | ":=" | "||" | "&&" | "==" | "===" | "<" | ">" | "!=" | "!==" | "<=" | ">="
  | "+" | "+." | "-" | "-." | "++" | "*" | "*." | "/" | "/." | "**" | "->"
  | "<>" | "%" | "|||" | "^^^" | "&&&" | "<<" | ">>" | ">>>" ->
    true
  | _ -> false

let not_ghost_operator operator (loc : Location.t) =
  is_binary_operator operator && not (loc.loc_ghost && operator = "++")

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
    when not_ghost_operator operator operator_loc ->
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
              | "res.await" | "res.inlineRecordDefinition" );
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
                    ({pexp_desc = Pexp_constant (Pconst_string payload)}, _);
              };
            ] ) ->
        String_literal.string_semantic payload <> "-4"
        || not (has_if_let_attribute attrs)
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
                    ({pexp_desc = Pexp_constant (Pconst_string payload)}, _);
              };
            ] ) ->
        String_literal.string_semantic payload <> "-4"
      | _ -> true)
    attrs

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
    || not (same_precedence_sub_expression operator lhs)
  | _ -> false

let should_inline_rhs_binary_expr rhs =
  match rhs.pexp_desc with
  | Parsetree.Pexp_constant _ | Pexp_let _ | Pexp_letmodule _
  | Pexp_letexception _ | Pexp_sequence _ | Pexp_open _ | Pexp_ifthenelse _
  | Pexp_for _ | Pexp_for_of _ | Pexp_for_await_of _ | Pexp_while _ | Pexp_try _
  | Pexp_array _ | Pexp_record _ ->
    true
  | _ -> false

let is_printable_attribute attr =
  match attr with
  | ( {
        Location.txt =
          ( "res.iflet" | "res.braces" | "ns.braces" | "JSX" | "res.await"
          | "res.ternary" | "res.inlineRecordDefinition" | "res.dictSpread" );
      },
      _ ) ->
    false
  | _ -> true

let has_printable_attributes attrs = List.exists is_printable_attribute attrs

let filter_printable_attributes attrs = List.filter is_printable_attribute attrs

let partition_printable_attributes attrs =
  List.partition is_printable_attribute attrs

let partition_doc_comment_attributes attrs =
  List.partition
    (fun ((id, payload) : Parsetree.attribute) ->
      match (id, payload) with
      | ( {txt = "res.doc"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string _)}, _);
              };
            ] ) ->
        true
      | _ -> false)
    attrs

let is_fun_expr expr =
  match expr.pexp_desc with
  | Pexp_fun _ -> true
  | _ -> false

let requires_special_callback_printing_last_arg args =
  let rec loop args =
    match args with
    | [] -> false
    | [(_, expr)] when is_fun_expr expr -> true
    | (_, expr) :: _ when is_fun_expr expr -> false
    | _ :: rest -> loop rest
  in
  loop args

let requires_special_callback_printing_first_arg args =
  let rec loop args =
    match args with
    | [] -> true
    | (_, expr) :: _ when is_fun_expr expr -> false
    | _ :: rest -> loop rest
  in
  match args with
  | [(_, expr)] when is_fun_expr expr -> false
  | (_, expr) :: rest when is_fun_expr expr -> loop rest
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

let is_template_literal expr =
  match expr.pexp_desc with
  | Pexp_template _ -> true
  | _ -> false

let is_tagged_template_literal expr =
  match expr.pexp_desc with
  | Pexp_tagged_template _ -> true
  | _ -> false

let has_spread_attr attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Location.txt = "res.spread"}, _ -> true
      | _ -> false)
    attrs

let is_spread_list expr =
  match expr.pexp_desc with
  | Pexp_ident
      {txt = Longident.Ldot (Longident.Lident "Primitive_list", "spread")} ->
    has_spread_attr expr.pexp_attributes
  | _ -> false

let is_spread_array expr =
  match expr.pexp_desc with
  | Pexp_ident
      {txt = Longident.Ldot (Longident.Lident "Primitive_array", "spread")} ->
    has_spread_attr expr.pexp_attributes
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

let collect_spread_dict_expr_parts expr =
  let extract_literal_dict_rows (expr : Parsetree.expression) =
    match expr with
    | {
     pexp_desc =
       Pexp_apply
         {
           funct =
             {
               pexp_desc =
                 Pexp_ident
                   {txt = Longident.Ldot (Lident "Primitive_dict", "make")};
             };
           args = [(Nolabel, key_values)];
         };
    }
      when is_tuple_array key_values ->
      Some key_values
    | _ -> None
  in
  let is_empty_tuple_array (expr : Parsetree.expression) =
    match expr.pexp_desc with
    | Pexp_array [] -> true
    | _ -> false
  in
  match expr with
  | {
   pexp_desc =
     Pexp_apply
       {
         funct =
           {
             pexp_desc =
               Pexp_ident
                 {txt = Longident.Ldot (Lident "Primitive_dict", "spread")};
             pexp_attributes;
           };
         args =
           [(Nolabel, target_expr); (Nolabel, {pexp_desc = Pexp_array sources})];
       };
  }
    when has_dict_spread_attribute pexp_attributes ->
    let to_part expr =
      match extract_literal_dict_rows expr with
      | Some rows_expr ->
        if is_empty_tuple_array rows_expr then None
        else Some (DictExprRows rows_expr)
      | None -> Some (DictExprSpread expr)
    in
    Some (List.filter_map to_part (target_expr :: sources))
  | _ -> None

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
   *   ->Dict.get("wm-property")
   *   ->Option.flatMap(JSON.decodeString)
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
          funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "->"}};
          args = [(Nolabel, _operand1); (Nolabel, _operand2)];
        } ->
      true
    | _ -> false
  in
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "->"}};
        args = [(Nolabel, operand1); (Nolabel, _operand2)];
      }
    when not (is_pipe_expr operand1) ->
    true
  | _ -> false

let is_underscore_apply_sugar expr =
  match expr.pexp_desc with
  | Pexp_fun
      {
        params =
          [
            {
              p_lbl = Nolabel;
              p_default = None;
              p_pat = {ppat_desc = Ppat_var {txt = "__x"}};
            };
          ];
        body = {pexp_desc = Pexp_apply _};
      } ->
    true
  | _ -> false

let is_rewritten_underscore_apply_sugar expr =
  match expr.pexp_desc with
  | Pexp_ident {txt = Longident.Lident "_"} -> true
  | _ -> false

let get_jsx_prop_loc = function
  | Parsetree.JSXPropPunning (_, name) -> name.loc
  | Parsetree.JSXPropValue (name, _, value) ->
    {name.loc with loc_end = value.pexp_loc.loc_end}
  | Parsetree.JSXPropSpreading (loc, _) -> loc

let container_element_closing_tag_loc
    (tag : Parsetree.jsx_closing_container_tag) =
  {
    tag.jsx_closing_container_tag_name.loc with
    loc_start = tag.jsx_closing_container_tag_start;
    loc_end = tag.jsx_closing_container_tag_end;
  }

(** returns the location of the /> token in a unary element *)
let unary_element_closing_token (expression_loc : Warnings.loc) =
  {
    expression_loc with
    loc_start =
      {
        expression_loc.loc_end with
        pos_cnum = expression_loc.loc_end.pos_cnum - 2;
        pos_bol = expression_loc.loc_end.pos_bol - 2;
      };
  }
