open Mocha
open Test_utils

// Polymorphic record fields: the field stays polymorphic at every use.
type identity = {id: 'a. 'a => 'a}
let useIdentity = ({id}) => (id(1), id("s"))

type mapper = {map: 'a 'b. (array<'a>, 'a => 'b) => array<'b>}
let arrayMapper = {map: (xs, f) => xs->Array.map(f)}

// Explicitly polymorphic let binding.
let singleton: 'a. 'a => array<'a> = x => [x]

// `constraint` ties type parameters together.
type withDefault<'a, 'b> = ('a, 'b) constraint 'b = array<'a>
let withDefault: withDefault<int, _> = (1, [2])

// `as 'a` names a type inside the annotation.
type rec intList = [#Nil | #Cons(int, intList)]

let rec sum = (l: intList) =>
  switch l {
  | #Nil => 0
  | #Cons(n, rest) => n + sum(rest)
  }

let keep = (x: [#A | #B] as 'a): 'a => x

// `#...t` matches every constructor of a polymorphic variant type.
type basic = [#Red | #Green]
type color = [basic | #Blue]

let describeColor = (c: color) =>
  switch c {
  | #...basic as b => "basic " ++ ((b: basic) :> string)
  | #Blue => "blue"
  }

// Private types: readable from outside, constructed only inside.
module Positive: {
  type t = private int
  let make: int => option<t>
} = {
  type t = int
  let make = n => n > 0 ? Some(n) : None
}

module Token: {
  type t = private Word(string) | Number(int)
  let parse: string => t
} = {
  type t = Word(string) | Number(int)

  let parse = s =>
    switch Int.fromString(s) {
    | Some(n) => Number(n)
    | None => Word(s)
    }
}

// Variance annotations allow coercing an abstract type.
module Box: {
  type t<+'a>
  let make: 'a => t<'a>
  let get: t<'a> => 'a
} = {
  type t<'a> = option<'a>
  let make = x => Some(x)
  let get = x => x->Option.getOrThrow
}

let widened: Box.t<[#A | #B]> = (Box.make(#A) :> Box.t<[#A | #B]>)

// Destructive substitution removes a type from a signature.
module type Container = {
  type elt
  type t
  let make: elt => t
  let toArray: t => array<elt>
}

module type IntContainer = Container with type elt := int

module IntContainer: IntContainer = {
  type t = array<int>
  let make = x => [x]
  let toArray = t => t
}

// `with module` and `module type of`.
module type Ord = {
  type t
  let compare: (t, t) => int
}

module IntOrd = {
  type t = int
  let compare = (a: int, b) => a - b
}

module type HasOrd = {
  module O: Ord
  let max: (O.t, O.t) => O.t
}

module IntMax: HasOrd with module O = IntOrd = {
  module O = IntOrd
  let max = (a, b) => O.compare(a, b) > 0 ? a : b
}

module type IntOrdType = module type of IntOrd
module IntOrdCopy: IntOrdType = IntOrd

// Higher-order and generative functors.
module type MakeOrd = (X: Ord) => Ord
module Apply = (F: MakeOrd, X: Ord) => F(X)

module Reverse = (X: Ord) => {
  type t = X.t
  let compare = (a, b) => X.compare(b, a)
}

module ReverseInt = Apply(Reverse, IntOrd)

module Counter = () => {
  let count = ref(0)

  let next = () => {
    count := count.contents + 1
    count.contents
  }
}

module Counter1 = Counter()
module Counter2 = Counter()

// First-class modules with a type parameter.
module type Value = {
  type t
  let value: t
  let show: t => string
}

let showValue = (type a, m: module(Value with type t = a)) => {
  module V = unpack(m)
  V.show(V.value)
}

let valueOf = (type a, m: module(Value with type t = a)): a => {
  module V = unpack(m)
  V.value
}

module IntValue = {
  type t = int
  let value = 3
  let show = n => Int.toString(n)
}

describe(__MODULE__, () => {
  test("polymorphic fields", () => {
    eq(__LOC__, (1, "s"), useIdentity({id: x => x}))
    eq(
      __LOC__,
      ([2], ["a!"]),
      (arrayMapper.map([1], x => x + 1), arrayMapper.map(["a"], s => s ++ "!")),
    )
    eq(__LOC__, ([1], ["a"]), (singleton(1), singleton("a")))
  })

  test("constraint and alias", () => {
    eq(__LOC__, (1, [2]), withDefault)
    eq(__LOC__, 3, sum(#Cons(1, #Cons(2, #Nil))))
    eq(__LOC__, #B, keep(#B))
  })

  test("type pattern", () => {
    eq(__LOC__, "basic Red", describeColor(#Red))
    eq(__LOC__, "blue", describeColor(#Blue))
  })

  test("private types", () => {
    switch Positive.make(3) {
    | Some(p) => eq(__LOC__, 4, (p :> int) + 1)
    | None => ok(__LOC__, false)
    }
    eq(__LOC__, None, Positive.make(0))
    eq(
      __LOC__,
      7,
      switch Token.parse("7") {
      | Number(n) => n
      | Word(_) => 0
      },
    )
  })

  test("variance", () => {
    eq(__LOC__, #A, Box.get(widened))
  })

  test("signatures", () => {
    eq(__LOC__, [1], IntContainer.make(1)->IntContainer.toArray)
    eq(__LOC__, 9, IntMax.max(3, 9))
    eq(__LOC__, 2, IntOrdCopy.compare(5, 3))
  })

  test("functors", () => {
    ok(__LOC__, ReverseInt.compare(1, 2) > 0)
    eq(__LOC__, 1, Counter1.next())
    eq(__LOC__, 2, Counter1.next())
    eq(__LOC__, 1, Counter2.next())
  })

  test("first-class modules", () => {
    eq(__LOC__, "3", showValue(module(IntValue)))
    eq(__LOC__, 3, valueOf(module(IntValue)))
  })
})
