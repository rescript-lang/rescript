type t = A | B | C | D | E

let toEnum = x =>
  switch x {
  | A => 0
  | B => 1
  | C => 2
  | D => 3
  | E => 4
  }

let toString = x =>
  switch x {
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  }

let bar = x =>
  switch x {
  | A => 10
  | B | C | D => 0
  | E => 10
  }

type b = True | False

let and_ = (x, y) =>
  switch (x, y) {
  | (True, False) => False
  | (False, True) => False
  | (False, False) => False
  | (True, True) => True
  }

let id = x =>
  switch x {
  | True => True
  | False => False
  }

let not_ = x =>
  switch x {
  | True => False
  | False => True
  }

type state =
  | Empty
  | Int1(int)
  | Int2(int)
let st = state =>
  switch state {
  | Empty => 0
  | Int2(intValue)
  | Int1(intValue) => 23
  }

type show = No | After(int) | Yes

let showToJs = x =>
  switch x {
  | Yes | After(_) => true
  | No => false
  }

let third = l =>
  switch l {
  | list{1, 2, 3} => true
  | _ => false
  }

type rec lst = Empty | Cons(int, lst)

let third2 = l =>
  switch l {
  | Cons(1, Cons(2, Cons(3, Empty))) => true
  | _ => false
  }

module CustomizeTags = {
  type t = | @as("dd") A | @as(12) B | C | @as("qq") D(int) | @as(42) E(int) | F(string)

  let foo = x =>
    switch x {
    | A => 1
    | B => 2
    | C => 3
    | D(_) => 4
    | E(_) => 5
    | F(_) => 6
    }

  let a = A
  let b = B
  let c = C
  let d = D(42)
  let e = E(0)
}

module MyUndefined = {
  type t<'a> = | @as(undefined) Undefined | @as(unboxed) Present('a)
  // Note: 'a must not have undefined as value
  // There can be only one with payload, with 1 argument, to use unboxed

  let undefined = Undefined

  let isUndefined = x => x == Undefined

  let plus = (x, y) =>
    switch (x, y) {
    | (Undefined, _) => y
    | (_, Undefined) => x
    | (Present(n), Present(m)) => Present(n + m)
    }
}

module MyNull = {
  type t<'a> = | @as(null) Null | @as(unboxed) Present('a)
  // Note: 'a must not have null as value
  // There can be only one with payload, with 1 argument, to use unboxed

  let null = Null

  let isNull = x => x == Null

  let plus = (x, y) =>
    switch (x, y) {
    | (Null, _) => y
    | (_, Null) => x
    | (Present(n), Present(m)) => Present(n + m)
    }
}

module MyNullable = {
  type t<'a> =
    | @as(null) Null
    | @as(undefined) Undefined
    | @as(unboxed) Present('a)
  // Note: 'a must not have null or undefined as value
  // There can be only one with payload, with 1 argument, to use unboxed

  let null = Null
  let undefined = Undefined

  let isNull = x => x == Null
  let isUndefined = x => x == Undefined

  let plus = (x, y) =>
    switch (x, y) {
    | (Null | Undefined, _) => y
    | (_, Null | Undefined) => x
    | (Present(x), Present(y)) => Present(x + y)
    }

  let kind = x =>
    switch x {
    | Null => "null"
    | Undefined => "undefined"
    | Present(_) => "present"
    }

  let expectSeven = plus(Present(3), Present(4))
  Js.log2("expect 7:", expectSeven)
}

module MyNullableExtended = {
  type t<'a> =
    | @as(null) Null
    | @as(undefined) Undefined
    | @as(unboxed) Present('a)
    | WhyNotAnotherOne
  // Note: 'a must be a not have null or something that's not an object as value
  // There can be only one with payload, with 1 argument, to use unboxed

  let null = Null
  let undefined = Undefined
  let whynot = WhyNotAnotherOne

  let isNull = x => x == Null
  let isUndefined = x => x == Undefined
  let isWhyNot = x => x == WhyNotAnotherOne

  type vector = {x: float, y: float}

  let plus = (x, y) =>
    switch (x, y) {
    | (Null | Undefined, _) => y
    | (_, Null | Undefined) => x
    | (WhyNotAnotherOne, _) | (_, WhyNotAnotherOne) => WhyNotAnotherOne
    | (Present({x: x1, y: y1}), Present({x: x2, y: y2})) => Present({x: x1 +. x2, y: y1 +. y2})
    }

  let kind = x =>
    switch x {
    | Null => "null"
    | Undefined => "undefined"
    | Present(_) => "present"
    | WhyNotAnotherOne => "whynot"
    }

  let expectSeven = plus(Present({x: 4., y: 3.}), Present({x: 3., y: 4.}))
  Js.log2("expect {x:7, y:7}:", expectSeven)
}
