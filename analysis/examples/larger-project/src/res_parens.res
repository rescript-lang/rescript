module ParsetreeViewer = Res_parsetree_viewer
type kind = Parenthesized | Braced(Location.t) | Nothing

let expr = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | _ =>
    switch expr {
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_constraint(_)} => Parenthesized
    | _ => Nothing
    }
  }
}

let callExpr = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | _ =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      if switch ParsetreeViewer.filterParsingAttrs(attrs) {
      | list{_, ..._} => true
      | list{} => false
      } =>
      Parenthesized
    | _ if ParsetreeViewer.isUnaryExpression(expr) || ParsetreeViewer.isBinaryExpression(expr) =>
      Parenthesized
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_fun(_)} if ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
    | {
        pexp_desc:
          Pexp_lazy(_)
          | Pexp_assert(_)
          | Pexp_fun(_)
          | Pexp_newtype(_)
          | Pexp_function(_)
          | Pexp_constraint(_)
          | Pexp_setfield(_)
          | Pexp_match(_)
          | Pexp_try(_)
          | Pexp_while(_)
          | Pexp_for(_)
          | Pexp_ifthenelse(_),
      } =>
      Parenthesized
    | _ => Nothing
    }
  }
}

let structureExpr = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | _
      if ParsetreeViewer.hasAttributes(expr.pexp_attributes) &&
      !ParsetreeViewer.isJsxExpression(expr) =>
      Parenthesized
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_constraint(_)} => Parenthesized
    | _ => Nothing
    }
  }
}

let unaryExprOperand = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      if switch ParsetreeViewer.filterParsingAttrs(attrs) {
      | list{_, ..._} => true
      | list{} => false
      } =>
      Parenthesized
    | expr if ParsetreeViewer.isUnaryExpression(expr) || ParsetreeViewer.isBinaryExpression(expr) =>
      Parenthesized
    | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
      Nothing
    | {pexp_desc: Pexp_fun(_)} if ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
    | {
        pexp_desc:
          Pexp_lazy(_)
          | Pexp_assert(_)
          | Pexp_fun(_)
          | Pexp_newtype(_)
          | Pexp_function(_)
          | Pexp_constraint(_)
          | Pexp_setfield(_)
          | Pexp_extension(_)
          | Pexp_match(_)
          | Pexp_try(_)
          | Pexp_while(_)
          | Pexp_for(_)
          | Pexp_ifthenelse(_),
      } =>
      Parenthesized
    | _ => Nothing
    }
  }
}

let binaryExprOperand = (~isLhs, expr) => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_fun(_)} if ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
    | {pexp_desc: Pexp_constraint(_) | Pexp_fun(_) | Pexp_function(_) | Pexp_newtype(_)} =>
      Parenthesized
    | expr if ParsetreeViewer.isBinaryExpression(expr) => Parenthesized
    | expr if ParsetreeViewer.isTernaryExpr(expr) => Parenthesized
    | {
        pexp_desc:
          Pexp_lazy(_)
          | Pexp_assert(_),
      } if isLhs =>
      Parenthesized
    | {Parsetree.pexp_attributes: attrs} =>
      if ParsetreeViewer.hasPrintableAttributes(attrs) {
        Parenthesized
      } else {
        Nothing
      }
    }
  }
}

let subBinaryExprOperand = (parentOperator, childOperator) => {
  let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
  let precChild = ParsetreeViewer.operatorPrecedence(childOperator)
  precParent > precChild ||
    ((precParent === precChild &&
      !ParsetreeViewer.flattenableOperators(parentOperator, childOperator)) ||
    /* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… */
    parentOperator == "||" && childOperator == "&&")
}

let rhsBinaryExprOperand = (parentOperator, rhs) =>
  switch rhs.Parsetree.pexp_desc {
  | Parsetree.Pexp_apply(
      {
        pexp_attributes: list{},
        pexp_desc: Pexp_ident({txt: Longident.Lident(operator), loc: operatorLoc}),
      },
      list{(_, _left), (_, _right)},
    )
    if ParsetreeViewer.isBinaryOperator(operator) && !(operatorLoc.loc_ghost && operator == "^") =>
    let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
    let precChild = ParsetreeViewer.operatorPrecedence(operator)
    precParent === precChild
  | _ => false
  }

let flattenOperandRhs = (parentOperator, rhs) =>
  switch rhs.Parsetree.pexp_desc {
  | Parsetree.Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident(operator), loc: operatorLoc})},
      list{(_, _left), (_, _right)},
    )
    if ParsetreeViewer.isBinaryOperator(operator) && !(operatorLoc.loc_ghost && operator == "^") =>
    let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
    let precChild = ParsetreeViewer.operatorPrecedence(operator)
    precParent >= precChild || rhs.pexp_attributes != list{}
  | Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}) => false
  | Pexp_fun(_) if ParsetreeViewer.isUnderscoreApplySugar(rhs) => false
  | Pexp_fun(_)
  | Pexp_newtype(_)
  | Pexp_setfield(_)
  | Pexp_constraint(_) => true
  | _ if ParsetreeViewer.isTernaryExpr(rhs) => true
  | _ => false
  }

let lazyOrAssertExprRhs = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      if switch ParsetreeViewer.filterParsingAttrs(attrs) {
      | list{_, ..._} => true
      | list{} => false
      } =>
      Parenthesized
    | expr if ParsetreeViewer.isBinaryExpression(expr) => Parenthesized
    | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
      Nothing
    | {pexp_desc: Pexp_fun(_)} if ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
    | {
        pexp_desc:
          Pexp_lazy(_)
          | Pexp_assert(_)
          | Pexp_fun(_)
          | Pexp_newtype(_)
          | Pexp_function(_)
          | Pexp_constraint(_)
          | Pexp_setfield(_)
          | Pexp_match(_)
          | Pexp_try(_)
          | Pexp_while(_)
          | Pexp_for(_)
          | Pexp_ifthenelse(_),
      } =>
      Parenthesized
    | _ => Nothing
    }
  }
}

let isNegativeConstant = constant => {
  let isNeg = txt => {
    let len = String.length(txt)
    len > 0 && (@doesNotRaise String.get)(txt, 0) == '-'
  }

  switch constant {
  | Parsetree.Pconst_integer(i, _) | Pconst_float(i, _) if isNeg(i) => true
  | _ => false
  }
}

let fieldExpr = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {Parsetree.pexp_attributes: attrs}
      if switch ParsetreeViewer.filterParsingAttrs(attrs) {
      | list{_, ..._} => true
      | list{} => false
      } =>
      Parenthesized
    | expr if ParsetreeViewer.isBinaryExpression(expr) || ParsetreeViewer.isUnaryExpression(expr) =>
      Parenthesized
    | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
      Nothing
    | {pexp_desc: Pexp_constant(c)} if isNegativeConstant(c) => Parenthesized
    | {pexp_desc: Pexp_fun(_)} if ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
    | {
        pexp_desc:
          Pexp_lazy(_)
          | Pexp_assert(_)
          | Pexp_extension(_)
          | Pexp_fun(_)
          | Pexp_newtype(_)
          | Pexp_function(_)
          | Pexp_constraint(_)
          | Pexp_setfield(_)
          | Pexp_match(_)
          | Pexp_try(_)
          | Pexp_while(_)
          | Pexp_for(_)
          | Pexp_ifthenelse(_),
      } =>
      Parenthesized
    | _ => Nothing
    }
  }
}

let setFieldExprRhs = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_constraint(_)} => Parenthesized
    | _ => Nothing
    }
  }
}

let ternaryOperand = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {
        Parsetree.pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ),
      } =>
      Nothing
    | {pexp_desc: Pexp_constraint(_)} => Parenthesized
    | {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)} =>
      let (_attrsOnArrow, _parameters, returnExpr) = ParsetreeViewer.funExpr(expr)
      switch returnExpr.pexp_desc {
      | Pexp_constraint(_) => Parenthesized
      | _ => Nothing
      }
    | _ => Nothing
    }
  }
}

let startsWithMinus = txt => {
  let len = String.length(txt)
  if len === 0 {
    false
  } else {
    let s = (@doesNotRaise String.get)(txt, 0)
    s == '-'
  }
}

let jsxPropExpr = expr =>
  switch expr.Parsetree.pexp_desc {
  | Parsetree.Pexp_let(_)
  | Pexp_sequence(_)
  | Pexp_letexception(_)
  | Pexp_letmodule(_)
  | Pexp_open(_) =>
    Nothing
  | _ =>
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {
          Parsetree.pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
          pexp_attributes: list{},
        } if startsWithMinus(x) =>
        Parenthesized
      | {
          Parsetree.pexp_desc:
            Pexp_ident(_)
            | Pexp_constant(_)
            | Pexp_field(_)
            | Pexp_construct(_)
            | Pexp_variant(_)
            | Pexp_array(_)
            | Pexp_pack(_)
            | Pexp_record(_)
            | Pexp_extension(_)
            | Pexp_letmodule(_)
            | Pexp_letexception(_)
            | Pexp_open(_)
            | Pexp_sequence(_)
            | Pexp_let(_)
            | Pexp_tuple(_),
          pexp_attributes: list{},
        } =>
        Nothing
      | {
          Parsetree.pexp_desc: Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
          pexp_attributes: list{},
        } =>
        Nothing
      | _ => Parenthesized
      }
    }
  }

let jsxChildExpr = expr =>
  switch expr.Parsetree.pexp_desc {
  | Parsetree.Pexp_let(_)
  | Pexp_sequence(_)
  | Pexp_letexception(_)
  | Pexp_letmodule(_)
  | Pexp_open(_) =>
    Nothing
  | _ =>
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | _ =>
      switch expr {
      | {
          Parsetree.pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
          pexp_attributes: list{},
        } if startsWithMinus(x) =>
        Parenthesized
      | {
          Parsetree.pexp_desc:
            Pexp_ident(_)
            | Pexp_constant(_)
            | Pexp_field(_)
            | Pexp_construct(_)
            | Pexp_variant(_)
            | Pexp_array(_)
            | Pexp_pack(_)
            | Pexp_record(_)
            | Pexp_extension(_)
            | Pexp_letmodule(_)
            | Pexp_letexception(_)
            | Pexp_open(_)
            | Pexp_sequence(_)
            | Pexp_let(_),
          pexp_attributes: list{},
        } =>
        Nothing
      | {
          Parsetree.pexp_desc: Pexp_constraint(
            {pexp_desc: Pexp_pack(_)},
            {ptyp_desc: Ptyp_package(_)},
          ),
          pexp_attributes: list{},
        } =>
        Nothing
      | expr if ParsetreeViewer.isJsxExpression(expr) => Nothing
      | _ => Parenthesized
      }
    }
  }

let binaryExpr = expr => {
  let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
  switch optBraces {
  | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
  | None =>
    switch expr {
    | {Parsetree.pexp_attributes: list{_, ..._}} as expr
      if ParsetreeViewer.isBinaryExpression(expr) =>
      Parenthesized
    | _ => Nothing
    }
  }
}

let modTypeFunctorReturn = modType =>
  switch modType {
  | {Parsetree.pmty_desc: Pmty_with(_)} => true
  | _ => false
  }

/* Add parens for readability:
       module type Functor = SetLike => Set with type t = A.t
     This is actually:
       module type Functor = (SetLike => Set) with type t = A.t
 */
let modTypeWithOperand = modType =>
  switch modType {
  | {Parsetree.pmty_desc: Pmty_functor(_) | Pmty_with(_)} => true
  | _ => false
  }

let modExprFunctorConstraint = modType =>
  switch modType {
  | {Parsetree.pmty_desc: Pmty_functor(_) | Pmty_with(_)} => true
  | _ => false
  }

let bracedExpr = expr =>
  switch expr.Parsetree.pexp_desc {
  | Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}) => false
  | Pexp_constraint(_) => true
  | _ => false
  }

let includeModExpr = modExpr =>
  switch modExpr.Parsetree.pmod_desc {
  | Parsetree.Pmod_constraint(_) => true
  | _ => false
  }

let arrowReturnTypExpr = typExpr =>
  switch typExpr.Parsetree.ptyp_desc {
  | Parsetree.Ptyp_arrow(_) => true
  | _ => false
  }

let patternRecordRowRhs = (pattern: Parsetree.pattern) =>
  switch pattern.ppat_desc {
  | Ppat_constraint({ppat_desc: Ppat_unpack(_)}, {ptyp_desc: Ptyp_package(_)}) => false
  | Ppat_constraint(_) => true
  | _ => false
  }

