// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Parser — type declaration parsing
// Tests that all type declaration forms parse correctly and produce types that
// behave correctly at runtime.

let eq = (a, b) => a == b

// ─── Primitive types ────────────────────────────────────────────────
let intVal: int = 42
Test.run(__POS_OF__("type: int"), intVal, eq, 42)

let floatVal: float = 3.14
Test.run(__POS_OF__("type: float"), floatVal > 3.0, eq, true)

let strVal: string = "hello"
Test.run(__POS_OF__("type: string"), strVal, eq, "hello")

let boolVal: bool = true
Test.run(__POS_OF__("type: bool"), boolVal, eq, true)

let charVal: char = 'A'
Test.run(__POS_OF__("type: char"), charVal, eq, 'A')

let unitVal: unit = ()
Test.run(__POS_OF__("type: unit"), unitVal, eq, ())

// ─── Type aliases ───────────────────────────────────────────────────
type myInt = int
let aliased: myInt = 10
Test.run(__POS_OF__("type alias: int"), aliased, eq, 10)

type stringPair = (string, string)
let pair: stringPair = ("a", "b")
let (pa, pb) = pair
Test.run(__POS_OF__("type alias: tuple fst"), pa, eq, "a")
Test.run(__POS_OF__("type alias: tuple snd"), pb, eq, "b")

// ─── Option type ────────────────────────────────────────────────────
let someVal: option<int> = Some(42)
let noneVal: option<int> = None
Test.run(
  __POS_OF__("type: option some"),
  switch someVal {
  | Some(v) => v
  | None => -1
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("type: option none"),
  switch noneVal {
  | Some(v) => v
  | None => -1
  },
  eq,
  -1,
)

// ─── Result type ────────────────────────────────────────────────────
let okVal: result<int, string> = Ok(42)
let errVal: result<int, string> = Error("fail")
Test.run(
  __POS_OF__("type: result ok"),
  switch okVal {
  | Ok(v) => v
  | Error(_) => -1
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("type: result error"),
  switch errVal {
  | Ok(_) => ""
  | Error(e) => e
  },
  eq,
  "fail",
)

// ─── Tuple types ────────────────────────────────────────────────────
type triple = (int, string, bool)
let t: triple = (1, "two", true)
let (t1, t2, t3) = t
Test.run(__POS_OF__("type: tuple 3 int"), t1, eq, 1)
Test.run(__POS_OF__("type: tuple 3 string"), t2, eq, "two")
Test.run(__POS_OF__("type: tuple 3 bool"), t3, eq, true)

// Large tuple
type quad = (int, int, int, int)
let q: quad = (1, 2, 3, 4)
let (q1, q2, q3, q4) = q
Test.run(__POS_OF__("type: quad sum"), q1 + q2 + q3 + q4, eq, 10)

// ─── Record types ───────────────────────────────────────────────────
type person = {name: string, age: int}
let p: person = {name: "Alice", age: 30}
Test.run(__POS_OF__("type: record name"), p.name, eq, "Alice")
Test.run(__POS_OF__("type: record age"), p.age, eq, 30)

// Record with mutable field
type counter = {mutable count: int, label: string}
let c: counter = {count: 0, label: "clicks"}
c.count = 5
Test.run(__POS_OF__("type: mutable record"), c.count, eq, 5)
Test.run(__POS_OF__("type: mutable record immutable"), c.label, eq, "clicks")

// Record with optional field
type config = {host: string, port?: int}
let cfg1: config = {host: "localhost"}
let cfg2: config = {host: "example.com", port: 8080}
Test.run(
  __POS_OF__("type: optional field absent"),
  switch cfg1.port {
  | Some(_) => true
  | None => false
  },
  eq,
  false,
)
Test.run(
  __POS_OF__("type: optional field present"),
  switch cfg2.port {
  | Some(p) => p
  | None => 0
  },
  eq,
  8080,
)

// ─── Variant types ──────────────────────────────────────────────────
type shape =
  | Circle(float)
  | Rectangle(float, float)
  | Triangle(float, float, float)
  | Point

let area = shape =>
  switch shape {
  | Circle(r) => Float.Constants.pi *. r *. r
  | Rectangle(w, h) => w *. h
  | Triangle(a, b, _c) => 0.5 *. a *. b // simplified
  | Point => 0.0
  }

Test.run(__POS_OF__("type: variant rect"), area(Rectangle(3.0, 4.0)) > 11.9, eq, true)
Test.run(__POS_OF__("type: variant point"), area(Point) == 0.0, eq, true)

// Variant with inline record
type event =
  | Click({x: int, y: int})
  | KeyPress({key: string, ctrl: bool})
  | Resize({width: int, height: int})

let describeEvent = e =>
  switch e {
  | Click({x, y}) => `click at ${Int.toString(x)},${Int.toString(y)}`
  | KeyPress({key, ctrl: true}) => `ctrl+${key}`
  | KeyPress({key}) => key
  | Resize({width, height}) => `${Int.toString(width)}x${Int.toString(height)}`
  }

Test.run(
  __POS_OF__("type: inline record click"),
  describeEvent(Click({x: 10, y: 20})),
  eq,
  "click at 10,20",
)
Test.run(
  __POS_OF__("type: inline record ctrl"),
  describeEvent(KeyPress({key: "s", ctrl: true})),
  eq,
  "ctrl+s",
)
Test.run(
  __POS_OF__("type: inline record no ctrl"),
  describeEvent(KeyPress({key: "a", ctrl: false})),
  eq,
  "a",
)

// ─── Polymorphic types ──────────────────────────────────────────────
type box<'a> = {contents: 'a}
let intBox: box<int> = {contents: 42}
let strBox: box<string> = {contents: "hello"}
Test.run(__POS_OF__("type: poly int"), intBox.contents, eq, 42)
Test.run(__POS_OF__("type: poly string"), strBox.contents, eq, "hello")

// Multiple type parameters
type pair2<'a, 'b> = {fst: 'a, snd: 'b}
let mixedPair: pair2<int, string> = {fst: 1, snd: "one"}
Test.run(__POS_OF__("type: multi param fst"), mixedPair.fst, eq, 1)
Test.run(__POS_OF__("type: multi param snd"), mixedPair.snd, eq, "one")

// ─── Polymorphic variants ───────────────────────────────────────────
type primaryColor = [#red | #green | #blue]
type extendedColor = [primaryColor | #yellow | #cyan | #magenta]

let colorName = (c: extendedColor) =>
  switch c {
  | #red => "red"
  | #green => "green"
  | #blue => "blue"
  | #yellow => "yellow"
  | #cyan => "cyan"
  | #magenta => "magenta"
  }

Test.run(__POS_OF__("type: polyvar primary"), colorName(#red), eq, "red")
Test.run(__POS_OF__("type: polyvar extended"), colorName(#cyan), eq, "cyan")

// Polymorphic variant coercion
let primary: primaryColor = #red
let extended: extendedColor = (primary :> extendedColor)
Test.run(__POS_OF__("type: polyvar coercion"), colorName(extended), eq, "red")

// ─── Recursive types ────────────────────────────────────────────────
type rec linkedList<'a> = Empty | Cons('a, linkedList<'a>)

let rec listLength = lst =>
  switch lst {
  | Empty => 0
  | Cons(_, rest) => 1 + listLength(rest)
  }

let myList = Cons(1, Cons(2, Cons(3, Empty)))
Test.run(__POS_OF__("type: recursive list length"), listLength(myList), eq, 3)

let rec listToArray = lst =>
  switch lst {
  | Empty => []
  | Cons(x, rest) => Array.concat([x], listToArray(rest))
  }
Test.run(__POS_OF__("type: recursive to array"), listToArray(myList), eq, [1, 2, 3])

// ─── Mutually recursive types ───────────────────────────────────────
type rec expr =
  | Num(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Neg(expr)

let rec eval = e =>
  switch e {
  | Num(n) => n
  | Add(a, b) => eval(a) + eval(b)
  | Mul(a, b) => eval(a) * eval(b)
  | Neg(a) => -eval(a)
  }

Test.run(__POS_OF__("type: expr eval simple"), eval(Num(5)), eq, 5)
Test.run(
  __POS_OF__("type: expr eval complex"),
  eval(Add(Mul(Num(2), Num(3)), Neg(Num(1)))),
  eq,
  5,
)

// ─── Function types ─────────────────────────────────────────────────
type transform = int => int
let inc: transform = x => x + 1
Test.run(__POS_OF__("type: fn type"), inc(5), eq, 6)

type binOp = (int, int) => int
let mul: binOp = (a, b) => a * b
Test.run(__POS_OF__("type: fn type binary"), mul(3, 4), eq, 12)

type predicate<'a> = 'a => bool
let isPositive: predicate<int> = x => x > 0
Test.run(__POS_OF__("type: fn type poly"), isPositive(5), eq, true)
Test.run(__POS_OF__("type: fn type poly false"), isPositive(-1), eq, false)

// ─── Abstract types via modules ─────────────────────────────────────
module Id: {
  type t
  let make: int => t
  let value: t => int
} = {
  type t = int
  let make = x => x
  let value = x => x
}

let id = Id.make(42)
Test.run(__POS_OF__("type: abstract via module"), Id.value(id), eq, 42)

// ─── Type with constraints ─────────────────────────────────────────
type comparison = Less | Equal | Greater

let compare = (a: int, b: int): comparison =>
  if a < b {
    Less
  } else if a > b {
    Greater
  } else {
    Equal
  }

Test.run(__POS_OF__("type: constrained fn less"), compare(1, 2), eq, Less)
Test.run(__POS_OF__("type: constrained fn equal"), compare(3, 3), eq, Equal)
Test.run(__POS_OF__("type: constrained fn greater"), compare(5, 4), eq, Greater)

// ─── Unboxed types ──────────────────────────────────────────────────
@unboxed type stringOrInt = String(string) | Int(int)

let showSI = (v: stringOrInt) =>
  switch v {
  | String(s) => s
  | Int(i) => Int.toString(i)
  }

Test.run(__POS_OF__("type: unboxed string"), showSI(String("hi")), eq, "hi")
Test.run(__POS_OF__("type: unboxed int"), showSI(Int(42)), eq, "42")

// ─── Nested parameterized types ─────────────────────────────────────
type tree2<'a> = Leaf2 | Branch2(tree2<'a>, 'a, tree2<'a>)

let rec treeToList = t =>
  switch t {
  | Leaf2 => []
  | Branch2(l, v, r) => Array.concatMany([treeToList(l), [v], treeToList(r)])
  }

let bt = Branch2(Branch2(Leaf2, 1, Leaf2), 2, Branch2(Leaf2, 3, Leaf2))
Test.run(__POS_OF__("type: parameterized tree"), treeToList(bt), eq, [1, 2, 3])
