let printExprFunParameters = (~uncurried, parameters) =>
  switch parameters {
  | list{([], Asttypes.Nolabel, None, {Parsetree.ppat_desc: Ppat_any})}
    when !uncurried =>
    Doc.text("_")
  | list{(
      [],
      Asttypes.Nolabel,
      None,
      {Parsetree.ppat_desc: Ppat_var(stringLoc)}
    )} when !uncurried =>
    Doc.text(stringLoc.txt)
  | list{(
      [],
      Nolabel,
      None,
      {ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None)}
    )} when !uncurried =>
    Doc.text("()")
  | parameters =>
    let lparen = if uncurried {
      Doc.text("(. ")
    } else {
      Doc.lparen
    }
    let shouldHug = ParsetreeViewer.parametersShouldHug(parameters)
    let printedParamaters = Doc.concat(list{
      if shouldHug {
        Doc.nil
      } else {
        Doc.softLine
      },
      Doc.join(
        ~sep=Doc.concat(list{Doc.comma, Doc.line}),
        List.map(printExpFunParameter, parameters),
      ),
      })
    Doc.group(
      Doc.concat(list{
        lparen,
        if shouldHug {
          printedParamaters
        } else {
          Doc.indent(printedParamaters)
        },
        if shouldHug {
          Doc.nil
        } else {
          Doc.concat(list{Doc.trailingComma, Doc.softLine})
        },
        Doc.rparen,
      }),
    )
  }

let isHuggableExpression = (expr) =>
  switch expr.pexp_desc {
  | Pexp_array(_)
  | Pexp_tuple(_)
  | Pexp_construct ({txt: Longident.Lident("::")}, _)
  | Pexp_construct ({txt: Longident.Lident("[]")}, _)
  | Pexp_extension ({txt: "obj"}, _)
  | Pexp_record(_) => true
  | _ => false
  }

  switch colour {
    | (Red | Blue) | ((Green | Purple) | Rosa) | (Black | White) | AnotherColoooooour => x

  }

let precedence = x => switch x {
  | HashEqual
  | ColonEqual => 1
  | Lor => 2
  | Land => 3
  | Equal
  | EqualEqual
  | EqualEqualEqual
  | LessThan
  | GreaterThan
  | BangEqual
  | BangEqualEqual
  | LessEqual
  | GreaterEqual
  | BarGreater => 4
  | Plus
  | PlusDot
  | Minus
  | MinusDot
  | PlusPlus => 5
  | Asterisk
  | AsteriskDot
  | Forwardslash
  | ForwardslashDot => 6
  | Exponentiation => 7
  | Hash
  | HashHash
  | MinusGreater => 8
  | Dot => 9
  | _ => 0
}

let first = switch first {
| Some(x) => x
| None => ("": format6<_>)
}

// spread over multiple lines, because author used multiple lines
switch a {
| B
| C | D => 1
| E => 2
}

// keep on one line, author kept it on one line
switch a {
| B | C | D => 1
| E => 2
}

// should naturally break over multiple lines
switch a {
| Bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb | Ccccccccccccccccccccccccccccccccccccccccccccccccccccc | Dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd | Eeeeeeeeeeeeee => 1
| E => 2
}
