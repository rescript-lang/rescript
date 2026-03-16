// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: Full compilation pipeline
// Tests complete .res → .js compilation by exercising complex patterns that
// span the entire compiler: parsing, type checking, lambda, and JS generation.

let eq = (a, b) => a == b

// ─── Recursive algorithms ──────────────────────────────────────────
let rec fibonacci = n =>
  if n <= 1 {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
Test.run(__POS_OF__("e2e: fibonacci 0"), fibonacci(0), eq, 0)
Test.run(__POS_OF__("e2e: fibonacci 1"), fibonacci(1), eq, 1)
Test.run(__POS_OF__("e2e: fibonacci 10"), fibonacci(10), eq, 55)
Test.run(__POS_OF__("e2e: fibonacci 20"), fibonacci(20), eq, 6765)

let rec factorial = n =>
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
Test.run(__POS_OF__("e2e: factorial 0"), factorial(0), eq, 1)
Test.run(__POS_OF__("e2e: factorial 5"), factorial(5), eq, 120)
Test.run(__POS_OF__("e2e: factorial 10"), factorial(10), eq, 3628800)

// ─── Binary search tree ─────────────────────────────────────────────
type rec bst = Empty | Node(bst, int, bst)

let rec insert = (tree, value) =>
  switch tree {
  | Empty => Node(Empty, value, Empty)
  | Node(left, v, right) =>
    if value < v {
      Node(insert(left, value), v, right)
    } else if value > v {
      Node(left, v, insert(right, value))
    } else {
      tree
    }
  }

let rec contains = (tree, value) =>
  switch tree {
  | Empty => false
  | Node(left, v, right) =>
    if value == v {
      true
    } else if value < v {
      contains(left, value)
    } else {
      contains(right, value)
    }
  }

let rec inorder = tree =>
  switch tree {
  | Empty => []
  | Node(left, v, right) => Array.concatMany([inorder(left), [v], inorder(right)])
  }

let rec size = tree =>
  switch tree {
  | Empty => 0
  | Node(left, _, right) => 1 + size(left) + size(right)
  }

let bst =
  Empty
  ->insert(5)
  ->insert(3)
  ->insert(7)
  ->insert(1)
  ->insert(4)
  ->insert(6)
  ->insert(8)
  ->insert(2)

Test.run(__POS_OF__("e2e bst: size"), size(bst), eq, 8)
Test.run(__POS_OF__("e2e bst: contains 5"), contains(bst, 5), eq, true)
Test.run(__POS_OF__("e2e bst: contains 1"), contains(bst, 1), eq, true)
Test.run(__POS_OF__("e2e bst: contains 8"), contains(bst, 8), eq, true)
Test.run(__POS_OF__("e2e bst: not contains 9"), contains(bst, 9), eq, false)
Test.run(__POS_OF__("e2e bst: inorder"), inorder(bst), eq, [1, 2, 3, 4, 5, 6, 7, 8])

// ─── Queue implementation ──────────────────────────────────────────
module Queue = {
  type t<'a> = {front: array<'a>, back: array<'a>}

  let make = () => {front: [], back: []}

  let enqueue = (q, item) => {front: q.front, back: Array.concat(q.back, [item])}

  let dequeue = q =>
    switch q.front {
    | [first, ...rest] => Some((first, {front: rest, back: q.back}))
    | [] =>
      switch q.back->Array.toReversed {
      | [first, ...rest] => Some((first, {front: rest, back: []}))
      | [] => None
      }
    }

  let size = q => Array.length(q.front) + Array.length(q.back)

  let isEmpty = q => size(q) == 0

  let peek = q =>
    switch q.front {
    | [first, ..._] => Some(first)
    | [] =>
      switch q.back {
      | [] => None
      | _ => q.back->Array.toReversed->Array.get(0)
      }
    }
}

let q = Queue.make()->Queue.enqueue(1)->Queue.enqueue(2)->Queue.enqueue(3)
Test.run(__POS_OF__("e2e queue: size"), Queue.size(q), eq, 3)
Test.run(__POS_OF__("e2e queue: peek"), Queue.peek(q), eq, Some(1))

let (item, q2) = Queue.dequeue(q)->Option.getExn
Test.run(__POS_OF__("e2e queue: dequeue 1"), item, eq, 1)
Test.run(__POS_OF__("e2e queue: size after"), Queue.size(q2), eq, 2)

let (item2, q3) = Queue.dequeue(q2)->Option.getExn
Test.run(__POS_OF__("e2e queue: dequeue 2"), item2, eq, 2)

let (item3, q4) = Queue.dequeue(q3)->Option.getExn
Test.run(__POS_OF__("e2e queue: dequeue 3"), item3, eq, 3)
Test.run(__POS_OF__("e2e queue: empty after"), Queue.isEmpty(q4), eq, true)
Test.run(__POS_OF__("e2e queue: dequeue empty"), Queue.dequeue(q4), eq, None)

// ─── State machine ─────────────────────────────────────────────────
type state = Idle | Loading | Success(string) | Error2(string)
type action = Fetch | Succeed(string) | Fail(string) | Reset

let transition = (state, action) =>
  switch (state, action) {
  | (Idle, Fetch) => Loading
  | (Loading, Succeed(data)) => Success(data)
  | (Loading, Fail(err)) => Error2(err)
  | (Error2(_), Reset) | (Success(_), Reset) => Idle
  | (state, _) => state // ignore invalid transitions
  }

let stateToString = s =>
  switch s {
  | Idle => "idle"
  | Loading => "loading"
  | Success(d) => `success:${d}`
  | Error2(e) => `error:${e}`
  }

let finalState =
  Idle
  ->transition(Fetch)
  ->transition(Succeed("data"))

Test.run(__POS_OF__("e2e state: success flow"), stateToString(finalState), eq, "success:data")

let errorState =
  Idle
  ->transition(Fetch)
  ->transition(Fail("network"))

Test.run(__POS_OF__("e2e state: error flow"), stateToString(errorState), eq, "error:network")

let resetState = errorState->transition(Reset)
Test.run(__POS_OF__("e2e state: reset"), stateToString(resetState), eq, "idle")

// Ignore invalid transitions
let invalidState = Idle->transition(Succeed("data"))
Test.run(__POS_OF__("e2e state: invalid"), stateToString(invalidState), eq, "idle")

// ─── Higher-order function composition ──────────────────────────────
let pipe2 = (f, g) => x => g(f(x))
let pipe3 = (f, g, h) => x => h(g(f(x)))

let transform = pipe3(x => x + 1, x => x * 2, x => x - 3)
Test.run(__POS_OF__("e2e pipe: compose3"), transform(5), eq, 9)

let pipeline = Array.fromInitializer(~length=10, i => i)
  ->Array.map(x => x * x)
  ->Array.filter(x => x > 10)
  ->Array.map(Int.toString)
  ->Array.join(", ")
Test.run(__POS_OF__("e2e pipe: full chain"), pipeline, eq, "16, 25, 36, 49, 64, 81")

// ─── Complex pattern matching ───────────────────────────────────────
type rec expr =
  | Lit(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Sub(expr, expr)
  | Div(expr, expr)
  | Neg(expr)
  | If(expr, expr, expr)

let rec eval = e =>
  switch e {
  | Lit(n) => n
  | Add(a, b) => eval(a) + eval(b)
  | Mul(a, b) => eval(a) * eval(b)
  | Sub(a, b) => eval(a) - eval(b)
  | Div(a, b) =>
    let bv = eval(b)
    bv == 0 ? 0 : eval(a) / bv
  | Neg(a) => -eval(a)
  | If(cond, t, f) => eval(cond) != 0 ? eval(t) : eval(f)
  }

Test.run(__POS_OF__("e2e expr: simple"), eval(Lit(42)), eq, 42)
Test.run(__POS_OF__("e2e expr: add"), eval(Add(Lit(3), Lit(4))), eq, 7)
Test.run(
  __POS_OF__("e2e expr: complex"),
  eval(Add(Mul(Lit(2), Lit(3)), Sub(Lit(10), Lit(4)))),
  eq,
  12,
)
Test.run(
  __POS_OF__("e2e expr: conditional"),
  eval(If(Lit(1), Lit(42), Lit(0))),
  eq,
  42,
)
Test.run(
  __POS_OF__("e2e expr: cond false"),
  eval(If(Lit(0), Lit(42), Lit(99))),
  eq,
  99,
)
Test.run(
  __POS_OF__("e2e expr: nested"),
  eval(
    If(
      Sub(Lit(5), Lit(5)),
      Lit(1),
      Add(Lit(2), Mul(Lit(3), Lit(4))),
    ),
  ),
  eq,
  14,
)

// ─── Complex module hierarchy ───────────────────────────────────────
module Geometry = {
  type point = {x: float, y: float}

  module Vector = {
    let make = (x, y): point => {x, y}
    let add = (a: point, b: point): point => {x: a.x +. b.x, y: a.y +. b.y}
    let scale = (v: point, s): point => {x: v.x *. s, y: v.y *. s}
    let length = (v: point) => Math.sqrt(v.x *. v.x +. v.y *. v.y)
    let dot = (a: point, b: point) => a.x *. b.x +. a.y *. b.y
  }

  module Shape = {
    type t = Circle(point, float) | Rect(point, float, float)

    let area = s =>
      switch s {
      | Circle(_, r) => Math.Constants.pi *. r *. r
      | Rect(_, w, h) => w *. h
      }

    let translate = (s, delta: point) =>
      switch s {
      | Circle(center, r) => Circle(Vector.add(center, delta), r)
      | Rect(origin, w, h) => Rect(Vector.add(origin, delta), w, h)
      }
  }
}

let v1 = Geometry.Vector.make(3.0, 4.0)
Test.run(
  __POS_OF__("e2e geometry: vector length"),
  Geometry.Vector.length(v1) > 4.99,
  eq,
  true,
)

let v2 = Geometry.Vector.make(1.0, 0.0)
let v3 = Geometry.Vector.make(0.0, 1.0)
Test.run(
  __POS_OF__("e2e geometry: dot product"),
  Geometry.Vector.dot(v2, v3) < 0.001,
  eq,
  true,
)

let circle = Geometry.Shape.Circle(Geometry.Vector.make(0.0, 0.0), 5.0)
Test.run(
  __POS_OF__("e2e geometry: circle area"),
  Geometry.Shape.area(circle) > 78.0,
  eq,
  true,
)

let rect = Geometry.Shape.Rect(Geometry.Vector.make(0.0, 0.0), 3.0, 4.0)
Test.run(
  __POS_OF__("e2e geometry: rect area"),
  Geometry.Shape.area(rect) > 11.99,
  eq,
  true,
)

// ─── Mutual recursion end-to-end ────────────────────────────────────
let rec isEvenRec = n =>
  if n == 0 {
    true
  } else {
    isOddRec(n - 1)
  }
and isOddRec = n =>
  if n == 0 {
    false
  } else {
    isEvenRec(n - 1)
  }

Test.run(__POS_OF__("e2e mutual: even 0"), isEvenRec(0), eq, true)
Test.run(__POS_OF__("e2e mutual: even 4"), isEvenRec(4), eq, true)
Test.run(__POS_OF__("e2e mutual: odd 3"), isOddRec(3), eq, true)
Test.run(__POS_OF__("e2e mutual: odd 4"), isOddRec(4), eq, false)

// ─── Exception pipeline ────────────────────────────────────────────
exception ParseError(string)
exception ValidationError(string)

let parseAndValidate = input => {
  try {
    let n = switch Int.fromString(input) {
    | Some(n) => n
    | None => raise(ParseError(`Cannot parse: ${input}`))
    }
    if n < 0 {
      raise(ValidationError("Must be non-negative"))
    }
    Ok(n)
  } catch {
  | ParseError(msg) => Error(`parse: ${msg}`)
  | ValidationError(msg) => Error(`validate: ${msg}`)
  }
}

Test.run(__POS_OF__("e2e exn: valid"), parseAndValidate("42"), eq, Ok(42))
Test.run(
  __POS_OF__("e2e exn: parse error"),
  parseAndValidate("abc"),
  eq,
  Error("parse: Cannot parse: abc"),
)
Test.run(
  __POS_OF__("e2e exn: validation error"),
  parseAndValidate("-5"),
  eq,
  Error("validate: Must be non-negative"),
)

// ─── Large variant dispatch ─────────────────────────────────────────
type httpStatus =
  | S100 | S101 | S200 | S201 | S202 | S204
  | S301 | S302 | S304 | S307 | S308
  | S400 | S401 | S403 | S404 | S405 | S409 | S410 | S413 | S415 | S422 | S429
  | S500 | S501 | S502 | S503 | S504

let statusCode = s =>
  switch s {
  | S100 => 100 | S101 => 101
  | S200 => 200 | S201 => 201 | S202 => 202 | S204 => 204
  | S301 => 301 | S302 => 302 | S304 => 304 | S307 => 307 | S308 => 308
  | S400 => 400 | S401 => 401 | S403 => 403 | S404 => 404 | S405 => 405
  | S409 => 409 | S410 => 410 | S413 => 413 | S415 => 415 | S422 => 422 | S429 => 429
  | S500 => 500 | S501 => 501 | S502 => 502 | S503 => 503 | S504 => 504
  }

Test.run(__POS_OF__("e2e dispatch: 200"), statusCode(S200), eq, 200)
Test.run(__POS_OF__("e2e dispatch: 404"), statusCode(S404), eq, 404)
Test.run(__POS_OF__("e2e dispatch: 500"), statusCode(S500), eq, 500)

let isSuccess = s =>
  switch s {
  | S200 | S201 | S202 | S204 => true
  | _ => false
  }
Test.run(__POS_OF__("e2e dispatch: is success"), isSuccess(S200), eq, true)
Test.run(__POS_OF__("e2e dispatch: not success"), isSuccess(S404), eq, false)

// ─── Memoization pattern ───────────────────────────────────────────
let memoize = f => {
  let cache = Dict.make()
  key => {
    switch Dict.get(cache, key) {
    | Some(v) => v
    | None =>
      let v = f(key)
      Dict.set(cache, key, v)
      v
    }
  }
}

let expensiveCompute = memoize(n => {
  // Simulate expensive computation
  let result = ref(0)
  for i in 1 to Int.fromString(n)->Option.getOr(0) {
    result := result.contents + i
  }
  result.contents
})

Test.run(__POS_OF__("e2e memo: first call"), expensiveCompute("100"), eq, 5050)
Test.run(__POS_OF__("e2e memo: cached call"), expensiveCompute("100"), eq, 5050)
Test.run(__POS_OF__("e2e memo: different key"), expensiveCompute("10"), eq, 55)
