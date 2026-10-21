open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

let numbers: unit => Generator.t<int, string, unit> = %raw(`function* () {
  yield 1
  yield 2
  yield 3
  return "done"
}`)

let words: unit => AsyncGenerator.t<string, unit, unit> = %raw(`async function* () {
  yield "a"
  yield "b"
}`)

describe(__MODULE__, () => {
  test("for..of iterates a generator", () => {
    let seen = []
    for x of numbers() {
      seen->Array.push(x * 10)
    }
    eq(__LOC__, [10, 20, 30], seen)
  })

  test("break in for..of over a generator", () => {
    let seen = []
    for x of numbers() {
      if x == 2 {
        break
      }
      seen->Array.push(x)
    }
    eq(__LOC__, [1], seen)
  })

  test("for..of iterates an iterator object", () => {
    let seen = []
    for x of [4, 5]->Array.values {
      seen->Array.push(x + 1)
    }
    eq(__LOC__, [5, 6], seen)
  })

  testAsync("for await..of iterates an async generator", async () => {
    let seen = []
    for await s of words() {
      seen->Array.push(s ++ "!")
    }
    eq(__LOC__, ["a!", "b!"], seen)
  })
})
