@@config({flags: ["-bs-gentype"]})

@genType
type t = | @as("thisIsA") A | @as(42) B | @as(null) C | D | @as(3.14) Pi

@genType
type tNU = | @as(null) N | @as(undefined) U

let toEnum = x =>
  switch x {
  | A => 0
  | B => 1
  | C => 2
  | D => 3
  | Pi => 5
  }

let toString = x =>
  switch x {
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | Pi => "Pi"
  }

let bar = x =>
  switch x {
  | A => 10
  | B | C | D => 0
  | Pi => 10
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
  type t = | @as("dd") A | @as(12) B | @as(false) C | @as("qq") D(int) | @as(42) E(int) | F(string)

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
  @genType @unboxed
  type t<'a> = | @as(undefined) Undefined | Present('a)
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
  @genType @unboxed
  type t<'a> = | @as(null) Null | Present('a)
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
  @genType @unboxed
  type t<'a> =
    | @as(null) Null
    | @as(undefined) Undefined
    | Present('a)
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
  Console.log2("expect 7:", expectSeven)
}

module MyNullableExtended = {
  @genType @unboxed
  type t<'a> =
    | @as(null) Null
    | @as(undefined) Undefined
    | Present('a)
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
  Console.log2("expect {x:7, y:7}:", expectSeven)
}

module TaggedUnions = {
  /*
  type Circle = {
    kind: 1; // Number literal
    radius: number;
  };

  type Square = {
    kind: "square"; // String literal
    sideLength: number;
  };

  type Rectangle = {
    kind: "rectangle"; // String literal
    width: number;
    height: number;
  };

  type Shape = Circle | Square | Rectangle;

  function area(shape: Shape): number {
    switch (shape.kind) {
      case 1: // Circle
        return Math.PI * shape.radius ** 2;
      case "square": // Square
        return shape.sideLength ** 2;
      case "rectangle": // Rectangle
        return shape.width * shape.height;
      default:
        throw new Error("Invalid shape kind");
    }
  }
*/
  @tag("kind")
  type shape =
    | @as(1) Circle({radius: float})
    | @as("square") Square({sideLength: float})
    | @as("rectangle") Rectangle({width: float, height: float})

  let area = (shape: shape): float => {
    switch shape {
    | Circle({radius}) => Js.Math._PI *. radius ** 2.
    | Square({sideLength}) => sideLength ** 2.
    | Rectangle({width, height}) => width *. height
    }
  }

  let circle = Circle({radius: 10.})
  let square = Square({sideLength: 10.})
}

module CustomTagNotInline = {
  @tag("custom-tag")
  type t = A(int) | B(int)
  let a = A(10)
  let b = B(20)
}

module UntaggedWithBool = {
  @unboxed @genType
  type t = String(string) | Float(float) | Bool(bool) | Object({name: string})

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Float(_) => "int"
    | Bool(true) => "true"
    | Bool(_) => "boolean"
    | Object({name}) => "Object" ++ name
    }
}

module UntaggedWithTuple = {
  @unboxed @genType
  type t = String(string) | Tuple((int, float, string))

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Tuple(_) => "tuple"
    }
}
