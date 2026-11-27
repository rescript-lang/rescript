open Mocha
open Test_utils

module Test_null = {
  let f1 = x =>
    switch Null.toOption(x) {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }

  let f2 = x => {
    let u = Null.toOption(x)
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f5 = (h, x) => {
    let u = Null.toOption(h(32))
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f4 = (h, x) => {
    let u = Null.toOption(h(32))
    let v = 32 + x
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, v)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f6 = (x, y) => x === y

  let f7 = x =>
    switch Some(x) {
    | None => None
    | Some(x) => x
    }

  /* can [from_opt x] generate [Some(None)] with a nested option type?
   No, if [x] is [null] then None else [Some x]
*/
  let f8 = (x: Null.t<Null.t<'a>>) =>
    switch Null.toOption(x) {
    | Some(x) =>
      switch Null.toOption(x) {
      | Some(_) => 0
      | None => 1
      }
    | None => 2
    }

  let u = f8(Null.make(Null.make(None)))

  let f9 = x => Null.toOption(x)

  let f10 = x => x == Null.null

  let f11 = Null.make(3) == Null.null
}

module Test_nullable = {
  let f1 = x =>
    switch Nullable.toOption(x) {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }

  let f2 = x => {
    let u = Nullable.toOption(x)
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f5 = (h, x) => {
    let u = Nullable.toOption(h(32))
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f4 = (h, x) => {
    let u = Nullable.toOption(h(32))
    let v = 32 + x
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, v)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f6 = (x, y) => x === y

  let f7 = x =>
    switch Some(x) {
    | None => None
    | Some(x) => x
    }

  /* can [from_opt x] generate [Some(None)] with a nested option type?
     No, if [x] is [null] then None else [Some x]
 */
  let f8 = (x: Nullable.t<Nullable.t<'a>>) =>
    switch Nullable.toOption(x) {
    | Some(x) =>
      switch Nullable.toOption(x) {
      | Some(_) => 0
      | None => 1
      }
    | None => 2
    }

  let u = f8(Nullable.make(Nullable.make(None)))

  let f9 = x => Nullable.toOption(x)

  let f10 = x => Nullable.isNullable(x)

  let f11 = Nullable.isNullable(Nullable.make(3))
}

describe(__MODULE__, () => {
  test("Test_nullable.f1 with return(0)", () => eq(__LOC__, Test_nullable.f1(Nullable.make(0)), 1))
  test("Test_nullable.f1 with null", () => eq(__LOC__, Test_nullable.f1(%raw("null")), 3))
  test("Test_nullable.f1 with undefined", () => eq(__LOC__, Test_nullable.f1(%raw("undefined")), 3))

  test("Test_null.f1 with return(0)", () => eq(__LOC__, Test_null.f1(Null.make(0)), 1))
  test("Test_null.f1 with null", () => eq(__LOC__, Test_null.f1(%raw("null")), 3))
})

module Null_undefined_neq = {
  let a = null
  let b = undefined
  let res = a != b
}
