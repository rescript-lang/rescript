let _ = Js.log(ImmutableArray.fromArray)
let fortytwo = 42

@genType
let fortyTwoButExported = 42

let thisIsUsedOnce = 34
ignore(thisIsUsedOnce)

let thisIsUsedTwice = 34
ignore(thisIsUsedTwice)
ignore(thisIsUsedTwice)

@dead
let thisIsMarkedDead = 99

let thisIsKeptAlive = 42

@live
let thisIsMarkedLive = thisIsKeptAlive

module Inner = {
  @dead
  let thisIsAlsoMarkedDead = 99
}

module M: {
  @dead
  let thisSignatureItemIsDead: int
} = {
  let thisSignatureItemIsDead = 34
}

module VariantUsedOnlyInImplementation: {
  type t = A // TODO: discovered this automatically
  let a: t
} = {
  type t = A
  let a = A
}

let _ = (x => x)(VariantUsedOnlyInImplementation.a)

let _ = DeadTypeTest.OnlyInInterface
let _ = DeadTypeTest.InBoth

type record = {
  xxx: int,
  yyy: int,
}

let _ = r => r.xxx
let _ = ({yyy}) => yyy

module UnderscoreInside = {
  let _ = 13
}

module MM: {
  let x: int
  let y: int
} = {
  let y = 55
  let x = y
  let valueOnlyInImplementation = 7
}

let _ = {
  Js.log(MM.x)
  44
}

let () = Js.log(DeadValueTest.valueAlive)

let rec unusedRec = () => unusedRec()

let rec split_map = l => {
  let _ = split_map(l)
  list{}
}

let rec rec1 = () => rec2()
and rec2 = () => rec1()

let rec recWithCallback = () => {
  let cb = () => recWithCallback()
  cb()
}

let rec foo = () => {
  let cb = () => bar()
  cb()
}
and bar = () => foo()

let withDefaultValue = (~paramWithDefault=3, y) => paramWithDefault + y

let () = Js.log(DeadRT.Root("xzz"))

module type LocalDynamicallyLoadedComponent2 = module type of DynamicallyLoadedComponent

module Chat = {}

let zzz = {
  let a1 = 1
  let a2 = 2
  let a3 = 3
}

let () = Js.log(<DynamicallyLoadedComponent s="" />)

let second = 1

let deadRef = ref(12)

@react.component
let make = (~s) => React.string(s)

let () = Js.log(make)

let theSideEffectIsLogging = Js.log(123)

let stringLengthNoSideEffects = String.length("sdkdl")

// Trace.infok("", "", ({pf}) => pf("%s", ""))

module GloobLive = {
  let globallyLive1 = 1
  let globallyLive2 = 2
  let globallyLive3 = 3
}

module WithInclude: {
  type t = A
} = {
  module T = {
    type t = A
  }
  include T
}

Js.log(WithInclude.A)

@dead
let funWithInnerVars = () => {
  let x = 34
  let y = 36
  x + y
}

type rc = {a: int}

@dead
let deadIncorrect = 34

let _ = deadIncorrect

type inlineRecord = IR({a: int, b: int, c: string, @dead d: int, @live e: int})

let ira = 10
let _ = ir =>
  switch ir {
  | IR({c} as r) => IR({a: ira, b: r.b, c, d: 0, e: 0})
  }

@dead
type inlineRecord2 = IR2({a: int, b: int})

type inlineRecord3 = | @dead IR3({a: int, b: int})
