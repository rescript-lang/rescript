open Mocha
open Test_utils

/***
An `@inline` function whose `switch` compiles to a static exit can be inlined
several times: each copy gets its own exit ids, so it runs its own handler.
*/

@inline(never) let id = x => x

@inline(never) let big = (s: string) => s ++ "!" ++ s

@inline
let pick = (x: int, y: int, flag: bool) =>
  switch (x, y) {
  | (0, _) | (_, 0) =>
    if flag {
      "t"
    } else {
      big("a") ++ big("b")
    }
  | _ => "other"
  }

let constant = pick(0, 5, true)
let computed = pick(id(0), id(5), id(false))
let computedOther = pick(id(1), id(5), id(true))
let computedLast = pick(id(3), id(0), id(true))

// Or-patterns nested in an or-pattern's handler: an inner exit inside an outer
// one's handler. The constant call folds both its handlers to a constant, the
// others must not run them.
@inline
let nested = (x: int, y: int, z: int, w: int, flag: bool) =>
  switch (x, y) {
  | (0, _) | (_, 0) =>
    switch (z, w) {
    | (0, _) | (_, 0) =>
      if flag {
        "t"
      } else {
        big("a") ++ big("b")
      }
    | _ => "inner"
    }
  | _ => "outer"
  }

let nestedConstant = nested(0, 5, 0, 5, true)
let nestedComputed = nested(id(0), id(5), id(0), id(5), id(false))
let nestedInner = nested(id(3), id(0), id(3), id(4), id(true))
let nestedOuter = nested(id(3), id(4), id(0), id(0), id(true))

exception Foo

@inline(never) let g = (x: int) => x == 7 ? throw(Foo) : x

// An `| exception` case compiles to a static exit with a negative id.
%%private(
  @inline
  let safe = (x: int, flag: bool) =>
    switch g(x) {
    | exception Foo => "foo"
    | _ =>
      if flag {
        "t"
      } else {
        big("a") ++ big("b")
      }
    }
)

let safeConstant = safe(0, true)
let safeComputed = safe(id(1), id(false))
let safeRaised = safe(id(7), id(false))

// An or-pattern binding a variable, under a guard.
@inline
let bound = (x: int, y: int, flag: bool) =>
  switch (x, y) {
  | (0, v) | (v, 5) if flag => big(Int.toString(v))
  | (0, _) | (_, 0) => "zero"
  | _ => "other"
  }

let boundConstant = bound(0, 5, true)
let boundComputed = bound(id(0), id(5), id(false))
let boundOther = bound(id(3), id(5), id(true))
let boundLast = bound(id(3), id(4), id(true))

// A functor body is copied once per application, without `@inline`.
module type S = {
  let v: int
  let w: int
  let flag: bool
}

module Make = (X: S) => {
  let r = switch (X.v, X.w) {
  | (0, _) | (_, 0) =>
    if X.flag {
      "t"
    } else {
      big("a") ++ big("b")
    }
  | _ => "other"
  }
}

module First = Make({
  let v = 0
  let w = 5
  let flag = true
})

module Second = Make({
  let v = id(0)
  let w = id(5)
  let flag = id(false)
})

describe(__MODULE__, () => {
  test("two inlined copies", () => {
    eq(__LOC__, constant, "t")
    eq(__LOC__, computed, "a!ab!b")
    eq(__LOC__, computedOther, "other")
    eq(__LOC__, computedLast, "t")
  })

  test("nested exits in inlined copies", () => {
    eq(__LOC__, nestedConstant, "t")
    eq(__LOC__, nestedComputed, "a!ab!b")
    eq(__LOC__, nestedInner, "inner")
    eq(__LOC__, nestedOuter, "outer")
  })

  test("inlined copies with an exception case", () => {
    eq(__LOC__, safeConstant, "t")
    eq(__LOC__, safeComputed, "a!ab!b")
    eq(__LOC__, safeRaised, "foo")
  })

  test("inlined copies of an or-pattern binding a variable", () => {
    eq(__LOC__, boundConstant, "5!5")
    eq(__LOC__, boundComputed, "zero")
    eq(__LOC__, boundOther, "3!3")
    eq(__LOC__, boundLast, "other")
  })

  test("functor applied twice", () => {
    eq(__LOC__, First.r, "t")
    eq(__LOC__, Second.r, "a!ab!b")
  })
})
