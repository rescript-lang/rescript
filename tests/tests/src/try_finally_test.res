open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

exception Boom(int)

describe(__MODULE__, () => {
  test("finally runs after the body", () => {
    let log = []
    let result = try {
      log->Array.push("body")
      1
    } finally {
      log->Array.push("finally")
    }
    eq(__LOC__, ["body", "finally"], log)
    eq(__LOC__, 1, result)
  })

  test("finally runs when the body throws", () => {
    let log = []
    let run = () =>
      try {
        log->Array.push("body")
        throw(Boom(1))
      } finally {
        log->Array.push("finally")
      }
    let caught = try run() catch {
    | Boom(n) => n
    }
    eq(__LOC__, ["body", "finally"], log)
    eq(__LOC__, 1, caught)
  })

  test("catch runs before finally", () => {
    let log = []
    let result = try {
      log->Array.push("body")
      throw(Boom(2))
    } catch {
    | Boom(n) =>
      log->Array.push("catch")
      n
    } finally {
      log->Array.push("finally")
    }
    eq(__LOC__, ["body", "catch", "finally"], log)
    eq(__LOC__, 2, result)
  })

  test("finally runs when the catch handler throws", () => {
    let log = []
    let run = () =>
      try {
        throw(Boom(3))
      } catch {
      | Boom(n) => throw(Boom(n + 1))
      } finally {
        log->Array.push("finally")
      }
    let caught = try run() catch {
    | Boom(n) => n
    }
    eq(__LOC__, ["finally"], log)
    eq(__LOC__, 4, caught)
  })

  test("an exception the catch does not match passes through finally", () => {
    let log = []
    let run = () =>
      try {
        throw(Boom(5))
      } catch {
      | Not_found => 0
      } finally {
        log->Array.push("finally")
      }
    let caught = try run() catch {
    | Boom(n) => n
    }
    eq(__LOC__, ["finally"], log)
    eq(__LOC__, 5, caught)
  })

  test("finally runs on break and continue", () => {
    let log = []
    for i in 0 to 5 {
      try {
        if i == 1 {
          continue
        }
        if i == 3 {
          break
        }
        log->Array.push(Int.toString(i))
      } finally {
        log->Array.push("f" ++ Int.toString(i))
      }
    }
    eq(__LOC__, ["0", "f0", "f1", "2", "f2", "f3"], log)
  })

  test("finally runs on break from a switch inside the try", () => {
    let log = []
    let i = ref(0)
    while i.contents < 5 {
      i := i.contents + 1
      try {
        switch i.contents {
        | 2 => continue
        | 4 => break
        | n => log->Array.push(Int.toString(n))
        }
      } finally {
        log->Array.push("f" ++ Int.toString(i.contents))
      }
    }
    eq(__LOC__, ["1", "f1", "f2", "3", "f3", "f4"], log)
  })

  test("loops inside finally can use break", () => {
    let log = []
    try {
      log->Array.push("body")
    } finally {
      for i in 0 to 5 {
        if i == 2 {
          break
        }
        log->Array.push(Int.toString(i))
      }
    }
    eq(__LOC__, ["body", "0", "1"], log)
  })

  test("nested finally blocks run innermost first", () => {
    let log = []
    let caught = try {
      try {
        try {
          throw(Boom(6))
        } finally {
          log->Array.push("inner")
        }
      } finally {
        log->Array.push("outer")
      }
    } catch {
    | Boom(n) => n
    }
    eq(__LOC__, ["inner", "outer"], log)
    eq(__LOC__, 6, caught)
  })

  // A self tail call under a finally must stay a real call: its finally
  // blocks run after the recursive call returns, innermost first.
  test("recursion under finally keeps call order", () => {
    let log = []
    let rec count = (n, ~retried) =>
      try {
        if n > 0 {
          count(n - 1, ~retried)
        } else if retried {
          0
        } else {
          throw(Boom(n))
        }
      } catch {
      | Boom(_) => count(n + 2, ~retried=true)
      } finally {
        log->Array.push(n)
      }
    eq(__LOC__, 0, count(2, ~retried=false))
    eq(__LOC__, [0, 1, 2, 0, 1, 2], log)
  })

  testAsync("finally runs after an awaited body", async () => {
    let log = []
    let run = async () =>
      try {
        let value = await Promise.resolve(7)
        log->Array.push("body")
        value
      } finally {
        log->Array.push("finally")
      }
    let result = await run()
    eq(__LOC__, ["body", "finally"], log)
    eq(__LOC__, 7, result)
  })

  testAsync("finally runs after a rejected await", async () => {
    let log = []
    let run = async () =>
      try {
        await Promise.reject(Boom(8))
      } finally {
        log->Array.push("finally")
      }
    let caught = try await run() catch {
    | Boom(n) => n
    }
    eq(__LOC__, ["finally"], log)
    eq(__LOC__, 8, caught)
  })
})
