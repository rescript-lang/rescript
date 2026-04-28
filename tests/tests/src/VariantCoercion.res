type a = One | @as("two") Two | Three

let a: a = Three

let b = (a :> string)

type onlyInts = | @as(1) One1 | @as(2) Two2 | @as(3) Three3

let i = One1

let d = (i :> int)

type onlyFloats = | @as(1.1) Onef | @as(2.2) Twof | @as(3.3) Threef

let ii = Onef

let dd = (ii :> float)

module CoerceVariants = {
  @unboxed type a = One(int) | @as(1.1) Two | @as(null) T2
  @unboxed type b = One(int) | @as(1.1) Two | @as(null) T2 | Three

  let a: a = Two

  let b: b = (a :> b)

  @tag("kind") type x = One({age: int, name?: string})
  @tag("kind") type y = One({age: int, name?: string}) | Two({two: string})

  let x: x = One({age: 1})
  let y: y = (x :> y)
}

module CoerceWithPayload = {
  @unboxed type strings = String(string) | First | Second | Third
  let a: strings = String("hello")
  let aa: strings = First
  let b: string = (a :> string)
  let bb: string = (aa :> string)

  @unboxed type floats = Number(float) | @as(1.) First | @as(2.) Second | @as(3.) Third
  let c: floats = Number(100.)
  let cc: floats = Second
  let d: float = (c :> float)
  let dd: float = (cc :> float)
}

module CoerceFromStringToVariant = {
  @unboxed type strings = String(string) | First | Second | Third
  let a = "hello"
  let aa = "First"
  let b: strings = (a :> strings)
  let bb: strings = (aa :> strings)

  @unboxed type mixed = String(string) | @as(1) One | @as(null) Null | Two
  let c = "Hi"
  let cc: mixed = (c :> mixed)
}

module CoerceFromIntToVariant = {
  @unboxed type ints = Int(int) | @as(1) First | @as(2) Second | @as(3) Third
  let a = 100
  let aa = 1
  let b: ints = (a :> ints)
  let bb: ints = (aa :> ints)

  @unboxed type mixed = Int(int) | @as(1) One | @as(null) Null | Two
  let c = 120
  let cc: mixed = (c :> mixed)
}

module CoerceFromFloatToVariant = {
  @unboxed type floats = Float(float) | @as(1.) First | @as(2.) Second | @as(3.) Third
  let a = 100.
  let aa = 1.
  let b: floats = (a :> floats)
  let bb: floats = (aa :> floats)

  @unboxed type mixed = Float(float) | @as(1.) One | @as(null) Null | Two
  let c = 120.
  let cc: mixed = (c :> mixed)
}

module CoerceFromBigintToVariant = {
  @unboxed type bigints = BigInt(bigint) | @as(1n) First | @as(2n) Second | @as(3n) Third
  let a = 100n
  let aa = 1n
  let b: bigints = (a :> bigints)
  let bb: bigints = (aa :> bigints)

  @unboxed type mixed = BigInt(bigint) | @as(1n) One | @as(null) Null | Two
  let c = 120n
  let cc: mixed = (c :> mixed)
}

module CoerceFromPolyvariantToVariant = {
  type simple = [#One | #Two]
  type simpleP = One | Two

  let simple: simple = #One
  let simpleP = (simple :> simpleP)

  type withAs = [#One | #two]
  type withAsP = One | @as("two") Two

  let withAs: withAs = #One
  let withAsP = (withAs :> withAsP)

  type withMoreVariantConstructors = [#One | #two]
  type withMoreVariantConstructorsP = One | @as("two") Two | Three

  let withMoreVariantConstructors: withMoreVariantConstructors = #One
  let withMoreVariantConstructorsP = (withMoreVariantConstructors :> withMoreVariantConstructorsP)

  type withUnboxedCatchAll = [#One | #someOtherThing]

  @unboxed
  type withUnboxedCatchAllP = One | @as("two") Two | Three | Other(string)

  let withUnboxedCatchAll: withUnboxedCatchAll = #One
  let withUnboxedCatchAllP = (withUnboxedCatchAll :> withUnboxedCatchAllP)
}

module CoerceVariantBinaryOp = {
  type flag = | @as(0) A | @as(2) B

  let x = 0->lor((B :> int))

  let v = B
  let f1 = () =>
    switch v {
    | A => "a"
    | B => "b"
    }
  let f2 = () =>
    switch (v :> int) {
    | 2 => "b"
    | _ => "a"
    }

  for x in 1 to (B :> int) {
    Console.log(x)
  }

  type flagStr = | @as("one") One | @as("two") Two

  let y = (One :> string)->String.length

  type flagFloat = | @as(1.5) X | @as(2.0) Y

  let z = (X :> float) +. (Y :> float) +. 1.5
}
