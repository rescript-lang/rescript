open Mocha
open Test_utils
open Constructor_explicit_arity

describe("constructor payload compatibility", () => {
  test("accepts both unary tuple spellings", () => {
    eq(__LOC__, readUnary(unary), 3)
    eq(__LOC__, readUnary(unaryUnparenthesized), 3)
    eq(__LOC__, readUnaryUnparenthesized(unary), 3)
    eq(__LOC__, readUnaryUnparenthesized(unaryUnparenthesized), 3)
  })
  test("accepts both binary constructor spellings", () => {
    eq(__LOC__, readBinary(binary), 3)
    eq(__LOC__, readBinary(binaryParenthesized), 3)
    eq(__LOC__, readBinaryParenthesized(binary), 3)
    eq(__LOC__, readBinaryParenthesized(binaryParenthesized), 3)
  })
  test("accepts unparenthesized option tuple payloads", () => {
    eq(__LOC__, readOptionUnparenthesized(optionUnparenthesized), 3)
    eq(__LOC__, readOptionUnparenthesized(Some((1, 2))), 3)
  })
})
