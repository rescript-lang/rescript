type binding = {
  name: string,
  value: int,
}

type evalError =
  | DivByZero
  | UnboundVar(string)

type stats = {
  mutable okCount: int,
  mutable errCount: int,
  mutable sum: int,
}

type program = {
  exprs: array<Expr.expr>,
  fallback: int,
}

type status = [#ok | #divByZero | #unbound]

@unboxed
type valueOrNote =
  | Int(int)
  | Note(string)

let lookup = (env: array<binding>, target: string): option<int> => {
  let found = ref(None)
  let i = ref(0)
  while i.contents < Array.length(env) && found.contents == None {
    switch env[i.contents] {
    | Some(b) if b.name == target => found := Some(b.value)
    | _ => ()
    }
    i := i.contents + 1
  }
  found.contents
}

let rec eval = (env: array<binding>, e: Expr.expr): result<int, evalError> =>
  switch e {
  | Lit(n) => Ok(n)
  | Var(name) =>
    switch lookup(env, name) {
    | Some(v) => Ok(v)
    | None => Error(UnboundVar(name))
    }
  | Neg(a) =>
    switch eval(env, a) {
    | Ok(v) => Ok(-v)
    | Error(err) => Error(err)
    }
  | Bin(op, a, b) =>
    switch (eval(env, a), eval(env, b)) {
    | (Error(err), _) | (_, Error(err)) => Error(err)
    | (Ok(x), Ok(y)) =>
      switch op {
      | Add => Ok(x + y)
      | Sub => Ok(x - y)
      | Mul => Ok(x * y)
      | Div =>
        if y == 0 {
          Error(DivByZero)
        } else {
          Ok(x / y)
        }
      }
    }
  }

let evalSimplified = (env: array<binding>, e: Expr.expr): result<int, evalError> =>
  eval(env, Expr.simplify(e))

let summarize = (res: result<int, evalError>): valueOrNote =>
  switch res {
  | Ok(v) => Int(v)
  | Error(DivByZero) => Note("div-by-zero")
  | Error(UnboundVar(name)) => Note("unbound:" ++ name)
  }

let classify = (res: result<int, evalError>): status =>
  switch res {
  | Ok(_) => #ok
  | Error(DivByZero) => #divByZero
  | Error(UnboundVar(_)) => #unbound
  }

let evalProgram = (env: array<binding>, prog: program): array<int> => {
  let out: array<int> = []
  for i in 0 to Array.length(prog.exprs) - 1 {
    switch prog.exprs[i] {
    | Some(e) =>
      let v = switch evalSimplified(env, e) {
      | Ok(n) => n
      | Error(_) => prog.fallback
      }
      Array.push(out, v)
    | None => ()
    }
  }
  out
}

let negateInPlace = (xs: array<int>): unit =>
  for i in 0 to Array.length(xs) - 1 {
    switch xs[i] {
    | Some(v) => xs[i] = -v
    | None => ()
    }
  }

let runBatch = (env: array<binding>, exprs: array<Expr.expr>): stats => {
  let st = {okCount: 0, errCount: 0, sum: 0}
  for i in 0 to Array.length(exprs) - 1 {
    switch exprs[i] {
    | Some(e) =>
      switch evalSimplified(env, e) {
      | Ok(v) =>
        st.okCount = st.okCount + 1
        st.sum = st.sum + v
      | Error(_) => st.errCount = st.errCount + 1
      }
    | None => ()
    }
  }
  st
}
