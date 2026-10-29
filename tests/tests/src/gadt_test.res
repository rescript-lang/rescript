open Mocha
open Test_utils

// A typed expression language: the index says what `eval` returns.
type rec expr<_> =
  | Int(int): expr<int>
  | Bool(bool): expr<bool>
  | Add(expr<int>, expr<int>): expr<int>
  | Equal(expr<int>, expr<int>): expr<bool>
  | If(expr<bool>, expr<'a>, expr<'a>): expr<'a>
  | Pair(expr<'a>, expr<'b>): expr<('a, 'b)>
  | First(expr<('a, 'b)>): expr<'a>

let rec eval: type a. expr<a> => a = expr =>
  switch expr {
  | Int(n) => n
  | Bool(b) => b
  | Add(a, b) => eval(a) + eval(b)
  | Equal(a, b) => eval(a) == eval(b)
  | If(cond, yes, no) => eval(cond) ? eval(yes) : eval(no)
  | Pair(a, b) => (eval(a), eval(b))
  | First(pair) =>
    let (a, _) = eval(pair)
    a
  }

// Type witnesses: one function, a different result type per witness.
type rec ty<_> =
  | TInt: ty<int>
  | TString: ty<string>
  | TList(ty<'a>): ty<list<'a>>
  | TPair(ty<'a>, ty<'b>): ty<('a, 'b)>
  | TOption(ty<'a>): ty<option<'a>>

let rec show: type a. (ty<a>, a) => string = (ty, value) =>
  switch (ty, value) {
  | (TInt, n) => Int.toString(n)
  | (TString, s) => `"${s}"`
  | (TList(_), list{}) => "[]"
  | (TList(ty), list{x, ...rest}) => show(ty, x) ++ "::" ++ show(TList(ty), rest)
  | (TPair(a, b), (x, y)) => "(" ++ show(a, x) ++ ", " ++ show(b, y) ++ ")"
  | (TOption(_), None) => "None"
  | (TOption(ty), Some(x)) => "Some(" ++ show(ty, x) ++ ")"
  }

let defaultOf: type a. ty<a> => a = ty =>
  switch ty {
  | TInt => 0
  | TString => ""
  | TList(_) => list{}
  | TPair(_, _) => throw(Invalid_argument("no default pair"))
  | TOption(_) => None
  }

// A match on a refined index needs no case for impossible constructors.
let intName = (ty: ty<int>) =>
  switch ty {
  | TInt => "int"
  }

// Equality witness and a runtime type equality check.
type rec equal<_, _> = Refl: equal<'a, 'a>

let cast = (type a b, Refl: equal<a, b>, x: a): b => x

let rec equalTy: type a b. (ty<a>, ty<b>) => option<equal<a, b>> = (a, b) =>
  switch (a, b) {
  | (TInt, TInt) => Some(Refl)
  | (TString, TString) => Some(Refl)
  | (TList(a), TList(b)) =>
    switch equalTy(a, b) {
    | Some(Refl) => Some(Refl)
    | None => None
    }
  | (TOption(a), TOption(b)) =>
    switch equalTy(a, b) {
    | Some(Refl) => Some(Refl)
    | None => None
    }
  | (TPair(a1, b1), TPair(a2, b2)) =>
    switch (equalTy(a1, a2), equalTy(b1, b2)) {
    | (Some(Refl), Some(Refl)) => Some(Refl)
    | _ => None
    }
  | _ => None
  }

// Existential types: the payload type is hidden from the outside.
type rec dynamic = Dynamic(ty<'a>, 'a): dynamic

let fromDynamic = (type a, ty: ty<a>, Dynamic(ty2, value)): option<a> =>
  switch equalTy(ty2, ty) {
  | Some(Refl) => Some(value)
  | None => None
  }

type rec showable = Showable({value: 'a, show: 'a => string}): showable

let showAll = items =>
  items->Array.map(item =>
    switch item {
    | Showable({value, show}) => show(value)
    }
  )

// A let destructuring a GADT constructor, with attributes in front.
let showOne = item => {
  /** Opens the existential. */
  @warning("-27")
  let Showable({value, show}) = item
  show(value)
}

// Existential packing a first-class module.
module type Show = {
  type t
  let show: t => string
}

type rec boxed = Boxed(module(Show with type t = 'a), 'a): boxed

module ShowInt = {
  type t = int
  let show = n => Int.toString(n)
}

let showBoxed = (Boxed(m, value)) => {
  module M = unpack(m)
  M.show(value)
}

// The index can drive the shape of a function: a typed printf.
type rec format<_> =
  | End: format<string>
  | Text(string, format<'a>): format<'a>
  | IntArg(format<'a>): format<int => 'a>
  | StringArg(format<'a>): format<string => 'a>

let rec format: type a. (format<a>, string) => a = (fmt, acc) =>
  switch fmt {
  | End => acc
  | Text(s, rest) => format(rest, acc ++ s)
  | IntArg(rest) => n => format(rest, acc ++ Int.toString(n))
  | StringArg(rest) => s => format(rest, acc ++ s)
  }

let sprintf = fmt => format(fmt, "")

// Runtime representation attributes work on GADT constructors.
type rec field<_> =
  | @as("int") IntField({@as("v") n: int}): field<int>
  | StringField({s: string}): field<string>
  | Empty: field<unit>

let fieldValue: type a. field<a> => a = field =>
  switch field {
  | IntField({n}) => n
  | StringField({s}) => s
  | Empty => ()
  }

@tag("kind")
type rec shape<_> =
  | Circle({radius: float}): shape<float>
  | Label({text: string}): shape<string>

let shapeValue: type a. shape<a> => a = shape =>
  switch shape {
  | Circle({radius}) => radius
  | Label({text}) => text
  }

@unboxed
type rec primitive<_> =
  | Number(float): primitive<float>
  | Str(string): primitive<string>

let primitiveValue: type a. primitive<a> => a = p =>
  switch p {
  | Number(f) => f
  | Str(s) => s
  }

// Polymorphic recursion on a nested (non-regular) type.
type rec nested<'a> = Flat('a) | Nest(nested<array<'a>>)

let rec depth: 'a. nested<'a> => int = n =>
  switch n {
  | Flat(_) => 0
  | Nest(n) => 1 + depth(n)
  }

describe(__MODULE__, () => {
  test("evaluator", () => {
    eq(__LOC__, 10, eval(If(Equal(Int(1), Add(Int(0), Int(1))), Int(10), Int(20))))
    eq(__LOC__, true, eval(First(Pair(Bool(true), Int(3)))))
    eq(__LOC__, (3, false), eval(Pair(Add(Int(1), Int(2)), Equal(Int(1), Int(2)))))
  })

  test("type witnesses", () => {
    eq(
      __LOC__,
      `(1, "a")::(2, "b")::[]`,
      show(TList(TPair(TInt, TString)), list{(1, "a"), (2, "b")}),
    )
    eq(__LOC__, "Some(None)", show(TOption(TOption(TInt)), Some(None)))
    eq(__LOC__, 0, defaultOf(TInt))
    eq(__LOC__, "", defaultOf(TString))
    eq(__LOC__, list{}, defaultOf(TList(TInt)))
    eq(__LOC__, None, defaultOf(TOption(TString)))
    eq(__LOC__, "int", intName(TInt))
  })

  test("equality witness", () => {
    eq(__LOC__, 42, cast(Refl, 42))
    ok(__LOC__, equalTy(TList(TInt), TList(TInt)) != None)
    ok(__LOC__, equalTy(TPair(TInt, TString), TPair(TInt, TInt)) == None)
  })

  test("existentials", () => {
    let values = [Dynamic(TInt, 1), Dynamic(TString, "x"), Dynamic(TList(TInt), list{1, 2})]
    eq(__LOC__, [Some(1), None, None], values->Array.map(v => fromDynamic(TInt, v)))
    eq(__LOC__, [None, None, Some(list{1, 2})], values->Array.map(v => fromDynamic(TList(TInt), v)))
    eq(
      __LOC__,
      ["1", "s"],
      showAll([
        Showable({value: 1, show: n => Int.toString(n)}),
        Showable({value: "s", show: s => s}),
      ]),
    )
    eq(__LOC__, "7", showBoxed(Boxed(module(ShowInt), 7)))
    eq(__LOC__, "8", showOne(Showable({value: 8, show: n => Int.toString(n)})))
  })

  test("typed printf", () => {
    eq(__LOC__, "x=1, y=b", sprintf(Text("x=", IntArg(Text(", y=", StringArg(End)))))(1)("b"))
  })

  test("representation attributes", () => {
    eq(__LOC__, 3, fieldValue(IntField({n: 3})))
    eq(__LOC__, "s", fieldValue(StringField({s: "s"})))
    eq(__LOC__, {"TAG": "int", "v": 3}, Obj.magic(IntField({n: 3})))
    eq(__LOC__, {"kind": "Circle", "radius": 1.5}, Obj.magic(Circle({radius: 1.5})))
    eq(__LOC__, "hi", shapeValue(Label({text: "hi"})))
    eq(__LOC__, 2., primitiveValue(Number(2.)))
    eq(__LOC__, "u", primitiveValue(Str("u")))
    eq(__LOC__, "u", Obj.magic(Str("u")))
  })

  test("polymorphic recursion", () => {
    eq(__LOC__, 2, depth(Nest(Nest(Flat([[1]])))))
  })
})
