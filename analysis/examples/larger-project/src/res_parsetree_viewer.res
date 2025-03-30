open Parsetree

let arrowType = ct => {
  let rec process = (attrsBefore, acc, typ) =>
    switch typ {
    | {ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2), ptyp_attributes: list{}} =>
      let arg = (list{}, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | {
        ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
        ptyp_attributes: list{({txt: "bs"}, _)} as attrs,
      } =>
      let arg = (attrs, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: _attrs} as returnType =>
      let args = List.rev(acc)
      (attrsBefore, args, returnType)
    | {
        ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
        ptyp_attributes: attrs,
      } =>
      let arg = (attrs, lbl, typ1)
      process(attrsBefore, list{arg, ...acc}, typ2)
    | typ => (attrsBefore, List.rev(acc), typ)
    }

  switch ct {
  | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: attrs} as typ =>
    process(attrs, list{}, {...typ, ptyp_attributes: list{}})
  | typ => process(list{}, list{}, typ)
  }
}

let functorType = modtype => {
  let rec process = (acc, modtype) =>
    switch modtype {
    | {pmty_desc: Pmty_functor(lbl, argType, returnType), pmty_attributes: attrs} =>
      let arg = (attrs, lbl, argType)
      process(list{arg, ...acc}, returnType)
    | modType => (List.rev(acc), modType)
    }

  process(list{}, modtype)
}

let processUncurriedAttribute = attrs => {
  let rec process = (uncurriedSpotted, acc, attrs) =>
    switch attrs {
    | list{} => (uncurriedSpotted, List.rev(acc))
    | list{({Location.txt: "bs"}, _), ...rest} => process(true, acc, rest)
    | list{attr, ...rest} => process(uncurriedSpotted, list{attr, ...acc}, rest)
    }

  process(false, list{}, attrs)
}

let collectListExpressions = expr => {
  let rec collect = (acc, expr) =>
    switch expr.pexp_desc {
    | Pexp_construct({txt: Longident.Lident("[]")}, _) => (List.rev(acc), None)
    | Pexp_construct(
        {txt: Longident.Lident("::")},
        Some({pexp_desc: Pexp_tuple(list{hd, tail})}),
      ) =>
      collect(list{hd, ...acc}, tail)
    | _ => (List.rev(acc), Some(expr))
    }

  collect(list{}, expr)
}

/* (__x) => f(a, __x, c) -----> f(a, _, c) */
let rewriteUnderscoreApply = expr =>
  switch expr.pexp_desc {
  | Pexp_fun(
      Nolabel,
      None,
      {ppat_desc: Ppat_var({txt: "__x"})},
      {pexp_desc: Pexp_apply(callExpr, args)} as e,
    ) =>
    let newArgs = List.map(arg =>
      switch arg {
      | (lbl, {pexp_desc: Pexp_ident({txt: Longident.Lident("__x")} as lid)} as argExpr) => (
          lbl,
          {...argExpr, pexp_desc: Pexp_ident({...lid, txt: Longident.Lident("_")})},
        )
      | arg => arg
      }
    , args)
    {...e, pexp_desc: Pexp_apply(callExpr, newArgs)}
  | _ => expr
  }

type funParamKind =
  | Parameter({
      attrs: Parsetree.attributes,
      lbl: Asttypes.arg_label,
      defaultExpr: option<Parsetree.expression>,
      pat: Parsetree.pattern,
    })
  | NewTypes({attrs: Parsetree.attributes, locs: list<Asttypes.loc<string>>})

let funExpr = expr => {
  /* Turns (type t, type u, type z) into "type t u z" */
  let rec collectNewTypes = (acc, returnExpr) =>
    switch returnExpr {
    | {pexp_desc: Pexp_newtype(stringLoc, returnExpr), pexp_attributes: list{}} =>
      collectNewTypes(list{stringLoc, ...acc}, returnExpr)
    | returnExpr => (List.rev(acc), returnExpr)
    }

  let rec collect = (attrsBefore, acc, expr) =>
    switch expr {
    | {
        pexp_desc: Pexp_fun(
          Nolabel,
          None,
          {ppat_desc: Ppat_var({txt: "__x"})},
          {pexp_desc: Pexp_apply(_)},
        ),
      } => (attrsBefore, List.rev(acc), rewriteUnderscoreApply(expr))
    | {pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr), pexp_attributes: list{}} =>
      let parameter = Parameter({
        attrs: list{},
        lbl: lbl,
        defaultExpr: defaultExpr,
        pat: pattern,
      })
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
      let (stringLocs, returnExpr) = collectNewTypes(list{stringLoc}, rest)
      let param = NewTypes({attrs: attrs, locs: stringLocs})
      collect(attrsBefore, list{param, ...acc}, returnExpr)
    | {
        pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
        pexp_attributes: list{({txt: "bs"}, _)} as attrs,
      } =>
      let parameter = Parameter({
        attrs: attrs,
        lbl: lbl,
        defaultExpr: defaultExpr,
        pat: pattern,
      })
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | {
        pexp_desc: Pexp_fun((Labelled(_) | Optional(_)) as lbl, defaultExpr, pattern, returnExpr),
        pexp_attributes: attrs,
      } =>
      let parameter = Parameter({
        attrs: attrs,
        lbl: lbl,
        defaultExpr: defaultExpr,
        pat: pattern,
      })
      collect(attrsBefore, list{parameter, ...acc}, returnExpr)
    | expr => (attrsBefore, List.rev(acc), expr)
    }

  switch expr {
  | {
      pexp_desc: Pexp_fun(Nolabel, _defaultExpr, _pattern, _returnExpr),
      pexp_attributes: attrs,
    } as expr =>
    collect(attrs, list{}, {...expr, pexp_attributes: list{}})
  | expr => collect(list{}, list{}, expr)
  }
}

let processBracesAttr = expr =>
  switch expr.pexp_attributes {
  | list{({txt: "ns.braces"}, _) as attr, ...attrs} => (
      Some(attr),
      {...expr, pexp_attributes: attrs},
    )
  | _ => (None, expr)
  }

let filterParsingAttrs = attrs => List.filter(attr =>
    switch attr {
    | (
        {
          Location.txt:
            "ns.ternary" | "ns.braces" | "res.template" | "bs" | "ns.iflet" | "ns.namedArgLoc",
        },
        _,
      ) => false
    | _ => true
    }
  , attrs)

let isBlockExpr = expr =>
  switch expr.pexp_desc {
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_let(_)
  | Pexp_open(_)
  | Pexp_sequence(_) => true
  | _ => false
  }

let isBracedExpr = expr =>
  switch processBracesAttr(expr) {
  | (Some(_), _) => true
  | _ => false
  }

let isMultilineText = txt => {
  let len = String.length(txt)
  let rec check = i =>
    if i >= len {
      false
    } else {
      let c = String.unsafe_get(txt, i)
      switch c {
      | '\n' | '\r' => true
      | '\\' =>
        if i + 2 == len {
          false
        } else {
          check(i + 2)
        }
      | _ => check(i + 1)
      }
    }

  check(0)
}

let isHuggableExpression = expr =>
  switch expr.pexp_desc {
  | Pexp_array(_)
  | Pexp_tuple(_)
  | Pexp_constant(Pconst_string(_, Some(_)))
  | Pexp_construct({txt: Longident.Lident("::" | "[]")}, _)
  | Pexp_extension({txt: "bs.obj" | "obj"}, _)
  | Pexp_record(_) => true
  | _ if isBlockExpr(expr) => true
  | _ if isBracedExpr(expr) => true
  | Pexp_constant(Pconst_string(txt, None)) if isMultilineText(txt) => true
  | _ => false
  }

let isHuggableRhs = expr =>
  switch expr.pexp_desc {
  | Pexp_array(_)
  | Pexp_tuple(_)
  | Pexp_construct({txt: Longident.Lident("::" | "[]")}, _)
  | Pexp_extension({txt: "bs.obj" | "obj"}, _)
  | Pexp_record(_) => true
  | _ if isBracedExpr(expr) => true
  | _ => false
  }

let isHuggablePattern = pattern =>
  switch pattern.ppat_desc {
  | Ppat_array(_)
  | Ppat_tuple(_)
  | Ppat_record(_)
  | Ppat_variant(_)
  | Ppat_construct(_) => true
  | _ => false
  }

let operatorPrecedence = operator =>
  switch operator {
  | ":=" => 1
  | "||" => 2
  | "&&" => 3
  | "=" | "==" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" => 4
  | "+" | "+." | "-" | "-." | "^" => 5
  | "*" | "*." | "/" | "/." => 6
  | "**" => 7
  | "#" | "##" | "->" => 8
  | _ => 0
  }

let isUnaryOperator = operator =>
  switch operator {
  | "~+" | "~+." | "~-" | "~-." | "not" => true
  | _ => false
  }

let isUnaryExpression = expr =>
  switch expr.pexp_desc {
  | Pexp_apply({pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})}, list{(Nolabel, _arg)})
    if isUnaryOperator(operator) => true
  | _ => false
  }

/* TODO: tweak this to check for ghost ^ as template literal */
let isBinaryOperator = operator =>
  switch operator {
  | ":="
  | "||"
  | "&&"
  | "="
  | "=="
  | "<"
  | ">"
  | "!="
  | "!=="
  | "<="
  | ">="
  | "|>"
  | "+"
  | "+."
  | "-"
  | "-."
  | "^"
  | "*"
  | "*."
  | "/"
  | "/."
  | "**"
  | "->"
  | "<>" => true
  | _ => false
  }

let isBinaryExpression = expr =>
  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident(operator), loc: operatorLoc})},
      list{(Nolabel, _operand1), (Nolabel, _operand2)},
    )
    if isBinaryOperator(operator) &&
    !(operatorLoc.loc_ghost && operator == "^") /* template literal */ => true
  | _ => false
  }

let isEqualityOperator = operator =>
  switch operator {
  | "=" | "==" | "<>" | "!=" => true
  | _ => false
  }

let flattenableOperators = (parentOperator, childOperator) => {
  let precParent = operatorPrecedence(parentOperator)
  let precChild = operatorPrecedence(childOperator)
  if precParent === precChild {
    !(isEqualityOperator(parentOperator) && isEqualityOperator(childOperator))
  } else {
    false
  }
}

let rec hasIfLetAttribute = attrs =>
  switch attrs {
  | list{} => false
  | list{({Location.txt: "ns.iflet"}, _), ..._} => true
  | list{_, ...attrs} => hasIfLetAttribute(attrs)
  }

let isIfLetExpr = expr =>
  switch expr {
  | {pexp_attributes: attrs, pexp_desc: Pexp_match(_)} if hasIfLetAttribute(attrs) => true
  | _ => false
  }

let hasAttributes = attrs => List.exists(attr =>
    switch attr {
    | ({Location.txt: "bs" | "res.template" | "ns.ternary" | "ns.braces" | "ns.iflet"}, _) => false
    /* Remove the fragile pattern warning for iflet expressions */
    | (
        {Location.txt: "warning"},
        PStr(list{{
          pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Pconst_string("-4", None))}, _),
        }}),
      ) =>
      !hasIfLetAttribute(attrs)
    | _ => true
    }
  , attrs)

let isArrayAccess = expr =>
  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
      list{(Nolabel, _parentExpr), (Nolabel, _memberExpr)},
    ) => true
  | _ => false
  }

type ifConditionKind =
  | If(Parsetree.expression)
  | IfLet(Parsetree.pattern, Parsetree.expression)

let collectIfExpressions = expr => {
  let rec collect = (acc, expr) =>
    switch expr.pexp_desc {
    | Pexp_ifthenelse(ifExpr, thenExpr, Some(elseExpr)) =>
      collect(list{(If(ifExpr), thenExpr), ...acc}, elseExpr)
    | Pexp_ifthenelse(ifExpr, thenExpr, None as elseExpr) =>
      let ifs = List.rev(list{(If(ifExpr), thenExpr), ...acc})
      (ifs, elseExpr)
    | Pexp_match(
        condition,
        list{
          {pc_lhs: pattern, pc_guard: None, pc_rhs: thenExpr},
          {pc_rhs: {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}},
        },
      ) if isIfLetExpr(expr) =>
      let ifs = List.rev(list{(IfLet(pattern, condition), thenExpr), ...acc})
      (ifs, None)
    | Pexp_match(
        condition,
        list{{pc_lhs: pattern, pc_guard: None, pc_rhs: thenExpr}, {pc_rhs: elseExpr}},
      ) if isIfLetExpr(expr) =>
      collect(list{(IfLet(pattern, condition), thenExpr), ...acc}, elseExpr)
    | _ => (List.rev(acc), Some(expr))
    }

  collect(list{}, expr)
}

let rec hasTernaryAttribute = attrs =>
  switch attrs {
  | list{} => false
  | list{({Location.txt: "ns.ternary"}, _), ..._} => true
  | list{_, ...attrs} => hasTernaryAttribute(attrs)
  }

let isTernaryExpr = expr =>
  switch expr {
  | {pexp_attributes: attrs, pexp_desc: Pexp_ifthenelse(_)} if hasTernaryAttribute(attrs) => true
  | _ => false
  }

let collectTernaryParts = expr => {
  let rec collect = (acc, expr) =>
    switch expr {
    | {pexp_attributes: attrs, pexp_desc: Pexp_ifthenelse(condition, consequent, Some(alternate))}
      if hasTernaryAttribute(attrs) =>
      collect(list{(condition, consequent), ...acc}, alternate)
    | alternate => (List.rev(acc), alternate)
    }

  collect(list{}, expr)
}

let parametersShouldHug = parameters =>
  switch parameters {
  | list{Parameter({attrs: list{}, lbl: Asttypes.Nolabel, defaultExpr: None, pat})}
    if isHuggablePattern(pat) => true
  | _ => false
  }

let filterTernaryAttributes = attrs => List.filter(attr =>
    switch attr {
    | ({Location.txt: "ns.ternary"}, _) => false
    | _ => true
    }
  , attrs)

let filterFragileMatchAttributes = attrs => List.filter(attr =>
    switch attr {
    | (
        {Location.txt: "warning"},
        PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Pconst_string("-4", _))}, _)}}),
      ) => false
    | _ => true
    }
  , attrs)

let isJsxExpression = expr => {
  let rec loop = attrs =>
    switch attrs {
    | list{} => false
    | list{({Location.txt: "JSX"}, _), ..._} => true
    | list{_, ...attrs} => loop(attrs)
    }

  switch expr.pexp_desc {
  | Pexp_apply(_) => loop(expr.Parsetree.pexp_attributes)
  | _ => false
  }
}

let hasJsxAttribute = attributes => {
  let rec loop = attrs =>
    switch attrs {
    | list{} => false
    | list{({Location.txt: "JSX"}, _), ..._} => true
    | list{_, ...attrs} => loop(attrs)
    }

  loop(attributes)
}

let shouldIndentBinaryExpr = expr => {
  let samePrecedenceSubExpression = (operator, subExpression) =>
    switch subExpression {
    | {
        pexp_desc: Pexp_apply(
          {pexp_desc: Pexp_ident({txt: Longident.Lident(subOperator)})},
          list{(Nolabel, _lhs), (Nolabel, _rhs)},
        ),
      } if isBinaryOperator(subOperator) =>
      flattenableOperators(operator, subOperator)
    | _ => true
    }

  switch expr {
  | {
      pexp_desc: Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list{(Nolabel, lhs), (Nolabel, _rhs)},
      ),
    } if isBinaryOperator(operator) =>
    isEqualityOperator(operator) ||
    (!samePrecedenceSubExpression(operator, lhs) ||
    operator == ":=")
  | _ => false
  }
}

let shouldInlineRhsBinaryExpr = rhs =>
  switch rhs.pexp_desc {
  | Parsetree.Pexp_constant(_)
  | Pexp_let(_)
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_sequence(_)
  | Pexp_open(_)
  | Pexp_ifthenelse(_)
  | Pexp_for(_)
  | Pexp_while(_)
  | Pexp_try(_)
  | Pexp_array(_)
  | Pexp_record(_) => true
  | _ => false
  }

let isPrintableAttribute = attr =>
  switch attr {
  | (
      {Location.txt: "bs" | "res.template" | "ns.ternary" | "ns.braces" | "ns.iflet" | "JSX"},
      _,
    ) => false
  | _ => true
  }

let hasPrintableAttributes = attrs => List.exists(isPrintableAttribute, attrs)

let filterPrintableAttributes = attrs => List.filter(isPrintableAttribute, attrs)

let partitionPrintableAttributes = attrs => List.partition(isPrintableAttribute, attrs)

let requiresSpecialCallbackPrintingLastArg = args => {
  let rec loop = args =>
    switch args {
    | list{} => false
    | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)})} => true
    | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ..._} => false
    | list{_, ...rest} => loop(rest)
    }

  loop(args)
}

let requiresSpecialCallbackPrintingFirstArg = args => {
  let rec loop = args =>
    switch args {
    | list{} => true
    | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ..._} => false
    | list{_, ...rest} => loop(rest)
    }

  switch args {
  | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)})} => false
  | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ...rest} => loop(rest)
  | _ => false
  }
}

let modExprApply = modExpr => {
  let rec loop = (acc, modExpr) =>
    switch modExpr {
    | {pmod_desc: Pmod_apply(next, arg)} => loop(list{arg, ...acc}, next)
    | _ => (acc, modExpr)
    }

  loop(list{}, modExpr)
}

let modExprFunctor = modExpr => {
  let rec loop = (acc, modExpr) =>
    switch modExpr {
    | {pmod_desc: Pmod_functor(lbl, modType, returnModExpr), pmod_attributes: attrs} =>
      let param = (attrs, lbl, modType)
      loop(list{param, ...acc}, returnModExpr)
    | returnModExpr => (List.rev(acc), returnModExpr)
    }

  loop(list{}, modExpr)
}

let rec collectPatternsFromListConstruct = (acc, pattern) => {
  open Parsetree
  switch pattern.ppat_desc {
  | Ppat_construct({txt: Longident.Lident("::")}, Some({ppat_desc: Ppat_tuple(list{pat, rest})})) =>
    collectPatternsFromListConstruct(list{pat, ...acc}, rest)
  | _ => (List.rev(acc), pattern)
  }
}

let hasTemplateLiteralAttr = attrs => List.exists(attr =>
    switch attr {
    | ({Location.txt: "res.template"}, _) => true
    | _ => false
    }
  , attrs)

let isTemplateLiteral = expr =>
  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident("^")})},
      list{(Nolabel, _), (Nolabel, _)},
    ) if hasTemplateLiteralAttr(expr.pexp_attributes) => true
  | Pexp_constant(Pconst_string(_, Some(""))) => true
  | Pexp_constant(_) if hasTemplateLiteralAttr(expr.pexp_attributes) => true
  | _ => false
  }

/* Blue | Red | Green -> [Blue; Red; Green] */
let collectOrPatternChain = pat => {
  let rec loop = (pattern, chain) =>
    switch pattern.ppat_desc {
    | Ppat_or(left, right) => loop(left, list{right, ...chain})
    | _ => list{pattern, ...chain}
    }

  loop(pat, list{})
}

let isSinglePipeExpr = expr => {
  /* handles:
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
   */
  let isPipeExpr = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("->" | "|>")})},
        list{(Nolabel, _operand1), (Nolabel, _operand2)},
      ) => true
    | _ => false
    }

  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident("->" | "|>")})},
      list{(Nolabel, operand1), (Nolabel, _operand2)},
    ) if !isPipeExpr(operand1) => true
  | _ => false
  }
}

let isUnderscoreApplySugar = expr =>
  switch expr.pexp_desc {
  | Pexp_fun(Nolabel, None, {ppat_desc: Ppat_var({txt: "__x"})}, {pexp_desc: Pexp_apply(_)}) => true
  | _ => false
  }

let isRewrittenUnderscoreApplySugar = expr =>
  switch expr.pexp_desc {
  | Pexp_ident({txt: Longident.Lident("_")}) => true
  | _ => false
  }

