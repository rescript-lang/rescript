@@warning("-39-48-27")

// A progress function will eventually terminate
let progress = {
  let counter = ref((100))
  () => {
    if counter.contents < 0 {
      assert false
    }
    counter := counter.contents - 1
  }
}

// Another progress function
let progress2 = progress

// A progress function can be taken from a module
module Progress = {
  module Nested = {
    let f = progress
  }
}

// Need to declare at least one progress function and one recursive definition
@progress(progress)
let rec justReturn = () => ()

@progress(progress)
let rec alwaysLoop = () => alwaysLoop() // infinite loop

@progress(progress)
let rec alwaysProgress = () => {
  // Terminates
  progress()
  alwaysProgress()
}

@progress(progress)
let rec alwaysProgressWrongOrder = () => {
  alwaysProgressWrongOrder()
  progress() // Oops: this is too late
}

@progress(progress)
let rec doNotAlias = () => {
  // Must not alias recursive functions
  let alias = doNotAlias
  alias()
}

@progress((progress, progress2))
let rec // Terminates as each branch makes progress
progressOnBothBranches = x => {
  if x > 3 {
    progress()
  } else {
    progress2()
  }
  progressOnBothBranches(x)
}

@progress(progress)
let rec // Loops as progress is only on one branch
progressOnOneBranch = x => {
  if x > 3 {
    progress()
  }
  progressOnOneBranch(x)
}

@progress(progress)
let rec // callParseFunction is parametric: it takes a parse function and calls it
testParametricFunction = x => {
  if x > 3 {
    progress()
  }
  testParametricFunction2(x)
}
and testParametricFunction2 = x => callParseFunction(x, ~parseFunction=testParametricFunction)
and callParseFunction = (x, ~parseFunction) => parseFunction(x) // loops

@progress(Progress.Nested.f)
let rec testCacheHit = x => {
  if x > 0 {
    doNothing(x)
    doNothing(x) // this should hit the analysis cache
    Progress.Nested.f()
  }
  testCacheHit(x)
}
and doNothing = _ => ()

@progress(progress)
let rec // Loops as can't rely on a specific evaluation order
evalOrderIsNotLeftToRight = x => {
  let combineTwoUnits = ((), ()) => ()
  combineTwoUnits(progress(), evalOrderIsNotLeftToRight(x))
}

@progress(progress)
let rec // Loops as can't rely on a specific evaluation order
evalOrderIsNotRightToLeft = x => {
  let combineTwoUnits = ((), ()) => ()
  combineTwoUnits(evalOrderIsNotRightToLeft(x), progress())
}

@progress(progress)
let rec // Terminates: all arguments are evaluated in some order
butFirstArgumentIsAlwaysEvaluated = x => {
  let combineTwoUnits = ((), ()) => ()
  combineTwoUnits(progress(), ())
  butFirstArgumentIsAlwaysEvaluated(x)
}

@progress(progress)
let rec // Terminates: all arguments are evaluated in some order
butSecondArgumentIsAlwaysEvaluated = x => {
  let combineTwoUnits = ((), ()) => ()
  combineTwoUnits((), progress())
  butSecondArgumentIsAlwaysEvaluated(x)
}

module Parser = {
  type token =
    | Asterisk
    | Eof
    | Lparen
    | Int(int)
    | Plus
    | Rparen

  type position = {
    lnum: int,
    cnum: int,
  }

  type t = {
    mutable position: position,
    mutable errors: list<string>,
    mutable token: token,
  }

  let tokenToString = token =>
    switch token {
    | Asterisk => "*"
    | Eof => "Eof"
    | Lparen => "("
    | Int(n) => string_of_int(n)
    | Plus => "+"
    | Rparen => ")"
    }

  let next = p => {
    p.token = true ? Eof : Int((1000))
    p.position = {lnum: (1000), cnum: (80)}
  }

  let err = (p, s) => p.errors = list{s, ...p.errors}

  let expect = (p, token) =>
    if p.token == token {
      next(p)
    } else {
      err(p, "expected token " ++ tokenToString(p.token))
    }
}

module Expr = {
  type rec t =
    | Int(int)
    | Plus(t, t)
}

let parseList = (p: Parser.t, ~f) => {
  let rec loop = (p: Parser.t) =>
    if p.token == Asterisk {
      list{}
    } else {
      let item = f(p)
      let l = loop(p)
      list{item, ...l}
    }
  loop(p)
}

let parseInt = (p: Parser.t) => {
  let res = switch p.token {
  | Int(n) => n
  | _ =>
    Parser.err(p, "integer expected")
    -1
  }
  Parser.next(p)
  res
}

@progress(Parser.next)
let rec parseListInt = p => parseList(p, ~f=parseInt)

@progress
and parseListListInt = p => parseList(p, ~f=parseListInt)

@progress
and parseExpression = (~x=4, p: Parser.t) =>
  switch p.token {
  | Lparen =>
    Parser.next(p)
    let e1 = parseExpression(p)
    Parser.expect(p, Plus)
    let e2 = parseExpression(p)
    Parser.expect(p, Lparen)
    Expr.Plus(e1, e2)
  | _ => Expr.Int(parseInt(p))
  }

@progress
and parseListExpression = p => parseList(p, ~f=(x => parseExpression(x)))

@progress
and parseListExpression2 = p => parseList(p, ~f=parseExpression(~x=7, ...))

@progress
and parseListIntTailRecursive = p => {
  let rec loop = (p: Parser.t, l) =>
    if p.token == Asterisk {
      l
    } else {
      loop(p, list{parseInt(p), ...l})
    }
  loop(p, list{})
}

@progress(progress)
let rec testLoopAfterProgress = () => {
  progress()
  loopAfterProgress()
}
and loopAfterProgress = () => loopAfterProgress()

module UITermination = {
  type state = int
  type setState = (~f: state => option<state>) => unit

  type onClick = unit => unit
  type dom

  let nothing: onClick = () => ()

  type div = (~text: string, ~onClick: onClick) => dom
  let div: div = (~text, ~onClick) => assert false

  let initState = n => n == 0 ? Some(42) : None
  let increment = n => Some(n + 1)

  let incrementOnClick = (~setState: setState): onClick => () => setState(~f=increment)

  let counter = (state: state, ~setState: setState) => {
    setState(~f=initState)
    div(~text=string_of_int(state), ~onClick=() => setState(~f=increment))
  }

  @progress(initState)
  let rec counterCompiled = (state: state) => {
    switch initState(state) {
    | None => ()
    | Some(newState) => ignore(counterCompiled(newState))
    }
    ignore(string_of_int(state))
  }

  and onClick1 = state =>
    switch increment(state) {
    | None => ()
    | Some(newState) => counterCompiled(newState)
    }

  let countRenders = (state: state, ~setState: setState) => {
    setState(~f=increment)
    div(~text="I have been rendered " ++ (string_of_int(state) ++ " times"), ~onClick=nothing)
  }

  @progress(initState)
  let rec countRendersCompiled = (state: state) => {
    switch increment(state) {
    | None => ()
    | Some(newState) => ignore(countRendersCompiled(newState))
    }
    ignore("I have been rendered " ++ (string_of_int(state) ++ " times"))
  }
}

module ParserWihtOptionals = {
  let parseListO = (p: Parser.t, ~f) => {
    let rec loop = nodes =>
      if p.token == Asterisk {
        Parser.next(p)
        list{}
      } else {
        switch f(p) {
        | None => nodes
        | Some(item) => loop(list{item, ...nodes})
        }
      }
    loop(list{})
  }

  let parseIntO = (p: Parser.t) =>
    switch p.token {
    | Int(n) =>
      Parser.next(p)
      Some(n)
    | _ =>
      Parser.err(p, "integer expected")
      None
    }

  @progress((Parser.next, Parser.next))
  let rec parseListIntO = p => parseListO(p, ~f=parseIntO)

  and alwaysReturnNone = (p: Parser.t) =>
    switch p.token {
    | Int(_) =>
      Parser.next(p)
      alwaysReturnNone(p)
    | _ => None
    }

  @progress
  and testAlwaysReturnNone = p => alwaysReturnNone(p)

  @progress
  and parseIntOWrapper = p => parseIntO(p)

  @progress
  and thisMakesNoProgress = (p: Parser.t, y) => {
    let x = None
    switch y {
    | Some(_) => x
    | _ =>
      Parser.next(p)
      Some(10)
    }
  }
}

module Riddle = {
  @progress(Parser.next)
  let rec f = (p: Parser.t) =>
    switch p.token {
    | Int(i) => g(p) + i
    | Eof => 0
    | _ =>
      Parser.next(p)
      f(p)
    }

  and gParam = (p: Parser.t, ~g) =>
    switch p.token {
    | Int(i) => g(p) + i
    | _ => f(p)
    }

  and g = p => {
    Parser.next(p)
    gParam(p, ~g)
  }
}

module TerminationTypes = {
  // p ::= P | N   (P progress, or N no progress)
  // r ::= p | {Some:p, None:p}    (result, in case of optional specify progress separately)
  // t ::= _ | t1=>r[f1,... fn]t2  (when called, the function makes progress or not
  // and calls f1,...,fn without making progeess first)
  // Abbreviations: omit empty [], and rhs _

  let rec f /* _=>P[g] */ = p => g(p)
  and g /* _=>P */ = p => {
    Parser.next(p)
    f(p)
  }

  let rec kleene0 /* (~f:_=>P, _) => P */ = (~f, p) => {
    f(p)
    kleene0(~f, p)
  }

  let union /* (~f:_=>{Some:P, None:N}, ~g:_=>{Some:P, None:N}, _) => {Some:P, None:N} */ = (
    ~f,
    ~g,
    p,
  ) =>
    switch f(p) {
    | None => g(p)
    | Some(x) => x
    }

  let concat /* (~f:_=>{Some:P, None:N}, ~g:_=>{Some:P, None:N}, _) => {Some:P, None:N} */ = (
    ~f,
    ~g,
    p,
  ) =>
    switch f(p) {
    | None => None
    | Some(x) =>
      switch g(p) {
      | None => None
      | Some(y) => Some(x ++ y)
      }
    }

  let rec kleene /* (~f:_=>{Some:P, None:N}, _) => N */ = (~f, p) =>
    switch f(p) {
    | None => list{}
    | Some(x) => list{x, ...kleene(~f, p)}
    }

  and one /* _=>{Some:P, None:N} */ = (p: Parser.t) =>
    switch p.token {
    | Int(1) =>
      Parser.next(p)
      Some("1")
    | _ => None
    }

  and two /* _=>{Some:P, None:N} */ = (p: Parser.t) =>
    switch p.token {
    | Int(2) =>
      Parser.next(p)
      Some("2")
    | _ => None
    }

  and oneTwo /* _=>{Some:P, None:N} */ = p => concat(~f=one, ~g=two, p)

  @progress(Parser.next)
  and oneTwoStar /* _=>N */ = p => kleene(~f=oneTwo, p)
}

@progress(progress)
let rec testTry = () => {
  try throw(Not_found) catch {
  | Not_found =>
    let _ = #abc(progress())
    testTry()
  | _ =>
    let _ = [(), progress(), ()]
    testTry()
  }
}
