@genType
type typeL = NonUnary(int, int)

@genType
let makeVariant = () => NonUnary(5, 3)

type typeC =
  | C(string)
  | D(string)
type typeB = {c: typeC}
type typeD = Int(int)
type typeE = int
type typeA<'a> =
  | A('a, int)
  | B('a, int)

type typeF<'a> =
  | F('a)
  | G('a)
type typeH =
  | H(typeD, int)
  | I(typeD, int)
type typeJ = J(typeD, typeD)
type typeK = K((typeD, typeD))

@genType
let makeABC = (): typeA<typeB> => A({c: C("a string")}, 5)

@genType
let makeBC = (): typeB => {c: C("a string")}

@genType
let makeAC = (): typeA<typeC> => A(C("a string"), 5)

@genType
let makeAD = (): typeA<typeD> => A(Int(3), 5)

@genType
let makeAE = (): typeA<typeE> => A(3, 5)

@genType
let makeFD = (): typeF<typeD> => F(Int(3))

@genType
let makeHD = (): typeH => H(Int(5), 5)

@genType
let makeJ = (): typeJ => J(Int(5), Int(3))

@genType
let makeK = (): typeK => K((Int(5), Int(3)))

@genType
type boxedBinary =
  | BB(typeD, int)
  | Z(int)
@genType
type unboxedBinary = UB(typeD, int)
@genType
type inline =
  | I({i: int, j: int})
  | J({i: int, j: int})
  | K(int, int)
  | L({"i": int, "j": int})

@genType
let testBoxedBinary = (_: boxedBinary) => 34

@genType
let testUnboxedBinary = (_: unboxedBinary) => 34

@genType
let testInline = x =>
  switch x {
  | I(q) => I({...q, i: q.i})
  | J(q) => J(q)
  | K(a, b) => K(b, a)
  | L(q) => L(q)
  }
