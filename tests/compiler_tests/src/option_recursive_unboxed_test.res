open Mocha
open Test_utils

// Whether option of a recursive @unboxed variant can be unboxed (#9018)

@unboxed type rec t = Recur(t) | End

@unboxed type rec a = A(b) | AEnd
@unboxed and b = B(a) | BEnd

// Can be undefined through its own case, so option of it stays boxed
@unboxed type rec u = Wrap(u) | @as(undefined) Missing

let someT = (x: t) => Some(x)
let someA = (x: a) => Some(x)
let someU = (x: u) => Some(x)

describe(__MODULE__, () => {
  test("a recursive variant that cannot be undefined", () => {
    eq(__LOC__, Obj.magic(someT(End)), "End")
    eq(__LOC__, Obj.magic(someA(A(B(AEnd)))), "AEnd")
  })

  test("a recursive variant that can be undefined", () => {
    eq(__LOC__, someU(Missing) == None, false)
    eq(__LOC__, someU(Wrap(Missing)) == None, false)
  })
})
