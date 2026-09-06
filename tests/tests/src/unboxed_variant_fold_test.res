open Mocha
open Test_utils

/***
Constant folding a match on an untagged variant must reach the same answer the
generated dispatch does. Constructor identity is not observable at runtime for
these types, so a fold that reads it can disagree with the emitted code.

Each case below is computed twice: once with the argument in place, so the
optimizer folds it, and once through [id], whose [@inline(never)] keeps the
constant away from the fold and leaves the real dispatch to answer. The two
must agree.
*/

@unboxed
type color =
  | @as("primary") Primary
  | @as("secondary") Secondary
  | Color(string)
let colorName = v =>
  switch v {
  | Color(n) => n
  | _ => "not Color"
  }

@unboxed type number = | @as(1) One | Number(int)
let numberName = v =>
  switch v {
  | One => "one"
  | Number(_) => "number"
  }

// No literal constructors at all, so no overlap is possible.
@unboxed type pure = I(int) | S(string)
let pureName = v =>
  switch v {
  | I(_) => "int"
  | S(_) => "string"
  }

@unboxed
type opt =
  | @as(null) Nothing
  | @as(undefined) Missing
  | Obj({x: int})
let optName = v =>
  switch v {
  | Nothing => "null"
  | Missing => "undef"
  | Obj(_) => "obj"
  }

@unboxed type flag = | @as(true) Yes | Str(string)
let flagName = v =>
  switch v {
  | Yes => "yes"
  | Str(_) => "str"
  }

// @as(1) and a float payload of 1.0 are one JavaScript number.
@unboxed type num = | @as(1) One2 | F(float)
let numName = v =>
  switch v {
  | One2 => "one"
  | F(_) => "float"
  }

// An untagged inline record stays an object rather than becoming its payload.
@unboxed type rec_ = | @as("empty") Empty | R({y: int})
let recName = v =>
  switch v {
  | Empty => "empty"
  | R(_) => "rec"
  }

// The overlapping literal is reachable only through the default arm.
@unboxed type c2 = | @as("a") A | @as("b") B | C(string)
let pick = v =>
  switch v {
  | C(s) => s
  | _ => "lit"
  }

// A payload that is itself an untagged constant.
@unboxed type inner = | @as("primary") P | I2(string)
@unboxed type outer = | @as("x") X | W(inner)
let outerName = v =>
  switch v {
  | X => "x"
  | W(_) => "w"
  }

// An ordinary boxed variant keeps its constructor identity, tag and all.
type boxed = | @as("primary") BoxedPrimary | BoxedColor(string)
let boxedName = v =>
  switch v {
  | BoxedPrimary => "not Color"
  | BoxedColor(n) => n
  }

// Tuple constants emit as arrays, and must not select the object case.
@unboxed type objects = Tuple((int, int)) | Record({x: int})
let objectName = value =>
  switch value {
  | Tuple(_) => "tuple"
  | Record(_) => "record"
  }

// The built-in empty-list constructor is the JavaScript number zero.
@unboxed type lists = | @as(0) Zero | Values(list<int>)
let listName = value =>
  switch value {
  | Zero => "zero"
  | Values(_) => "list"
  }

// Bigint constructor spellings can differ while their runtime values agree.
@unboxed type bigintInner = | @as(1_0n) DecimalTen
@unboxed type bigintOuter = | @as(10n) BigOne | Wrapped(bigintInner)
let bigintName = value =>
  switch value {
  | BigOne => "one"
  | Wrapped(_) => "wrapped"
  }

// Integer constructor tags use the same 32-bit representation as int values.
@unboxed type wide = | @as(4294967297) WideOne | WideInt(int)
let wideName = value =>
  switch value {
  | WideOne => "one"
  | WideInt(_) => "int"
  }

@inline(never) let id = x => x

describe(__MODULE__, () => {
  test("folding agrees with runtime dispatch", () => {
    eq(__LOC__, colorName(Color("primary")), colorName(id(Color("primary"))))
    eq(__LOC__, colorName(Color("secondary")), colorName(id(Color("secondary"))))
    eq(__LOC__, colorName(Color("blue")), colorName(id(Color("blue"))))
    eq(__LOC__, colorName(Primary), colorName(id(Primary)))
    eq(__LOC__, numberName(Number(1)), numberName(id(Number(1))))
    eq(__LOC__, numberName(Number(2)), numberName(id(Number(2))))
    eq(__LOC__, numberName(One), numberName(id(One)))
    eq(__LOC__, pureName(I(1)), pureName(id(I(1))))
    eq(__LOC__, pureName(S("x")), pureName(id(S("x"))))
    eq(__LOC__, optName(Obj({x: 1})), optName(id(Obj({x: 1}))))
    eq(__LOC__, optName(Nothing), optName(id(Nothing)))
    eq(__LOC__, optName(Missing), optName(id(Missing)))
    eq(__LOC__, flagName(Str("true")), flagName(id(Str("true"))))
    eq(__LOC__, flagName(Yes), flagName(id(Yes)))
    eq(__LOC__, numName(F(1.0)), numName(id(F(1.0))))
    eq(__LOC__, numName(F(2.5)), numName(id(F(2.5))))
    eq(__LOC__, numName(One2), numName(id(One2)))
    eq(__LOC__, recName(R({y: 1})), recName(id(R({y: 1}))))
    eq(__LOC__, recName(Empty), recName(id(Empty)))
    eq(__LOC__, pick(C("a")), pick(id(C("a"))))
    eq(__LOC__, pick(C("z")), pick(id(C("z"))))
    eq(__LOC__, pick(A), pick(id(A)))
    eq(__LOC__, outerName(W(P)), outerName(id(W(P))))
    eq(__LOC__, outerName(W(I2("x"))), outerName(id(W(I2("x")))))
    eq(__LOC__, outerName(X), outerName(id(X)))
    eq(__LOC__, boxedName(BoxedColor("primary")), boxedName(id(BoxedColor("primary"))))
    eq(__LOC__, boxedName(BoxedPrimary), boxedName(id(BoxedPrimary)))
    eq(__LOC__, objectName(Tuple((1, 2))), objectName(id(Tuple((1, 2)))))
    eq(__LOC__, objectName(Record({x: 1})), objectName(id(Record({x: 1}))))
    eq(__LOC__, listName(Values(list{})), listName(id(Values(list{}))))
    eq(__LOC__, listName(Values(list{1})), listName(id(Values(list{1}))))
    eq(__LOC__, bigintName(Wrapped(DecimalTen)), bigintName(id(Wrapped(DecimalTen))))
    eq(__LOC__, wideName(WideInt(1)), wideName(id(WideInt(1))))
  })

  test("the folded answers themselves are correct", () => {
    eq(__LOC__, colorName(Color("primary")), "not Color")
    eq(__LOC__, colorName(Color("blue")), "blue")
    eq(__LOC__, numberName(Number(1)), "one")
    eq(__LOC__, numberName(Number(2)), "number")
    eq(__LOC__, pureName(I(1)), "int")
    eq(__LOC__, numName(F(1.0)), "one")
    eq(__LOC__, pick(C("a")), "lit")
    eq(__LOC__, outerName(W(I2("x"))), "x")
    eq(__LOC__, boxedName(BoxedColor("primary")), "primary")
    eq(__LOC__, objectName(Tuple((1, 2))), "tuple")
    eq(__LOC__, listName(Values(list{})), "zero")
    eq(__LOC__, bigintName(Wrapped(DecimalTen)), "one")
    eq(__LOC__, wideName(WideInt(1)), "one")
  })
})
