open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// The extension is a temporary encoding, independent of eventual surface syntax.
let choose = x => {
  if x > 0 {
    %return(42)
  }
  7
}

@inline
let alwaysReturn = x => %return(x)

let identity = x => {
  if true {
    %return(x)
  }
  x
}

let inLoops = target => {
  for i in 0 to 3 {
    let j = ref(0)
    while j.contents < 3 {
      j.contents = j.contents + 1
      switch target {
      | "first" => %return(i + j.contents)
      | "second" =>
        if i == 2 {
          %return(20)
        }
      | "third" => continue
      | _ => break
      }
    }
  }
  99
}

let returnAsync = async early => {
  if early {
    %return(await Promise.resolve(42))
  }
  7
}

type rec value<'a> = Int(int): value<int> | String(string): value<string>
let getGadt = (type a, v: value<a>): a => {
  switch v {
  | Int(x) => %return(x)
  | String(x) => %return(x)
  }
}

exception OperandFailure

let inTry = fail => {
  try {
    %return(
      if fail {
        throw(OperandFailure)
      } else {
        42
      }
    )
  } catch {
  | OperandFailure => %return(7)
  }
}

describe(__MODULE__, () => {
  test("early and implicit results", () => {
    eq(__LOC__, 42, getGadt(Int(42)))
    eq(__LOC__, "ok", getGadt(String("ok")))
    eq(__LOC__, 42, choose(1))
    eq(__LOC__, 7, choose(0))
    eq(__LOC__, 42, identity(42))
    eq(__LOC__, "ok", identity("ok"))
  })

  test("calls preserve the callee's return boundary", () => {
    eq(__LOC__, 43, alwaysReturn(42) + 1)
    eq(__LOC__, 43, Early_return_helper.first(1) + 1)
    eq(__LOC__, 8, Early_return_helper.first(0) + 1)
    let outer = () => {
      let inner = () => %return("inner")
      eq(__LOC__, "inner", inner())
      %return(42)
    }
    eq(__LOC__, 42, outer())
    eq(__LOC__, 43, (() => %return(42))() + 1)
  })

  test("return from nested loops and switches", () => {
    eq(__LOC__, 1, inLoops("first"))
    eq(__LOC__, 20, inLoops("second"))
    eq(__LOC__, 99, inLoops("third"))
    eq(__LOC__, 99, inLoops("other"))
  })

  test("return in while conditions stays in the source function", () => {
    let f = limit => {
      let i = ref(0)
      while {
        if i.contents == limit {
          %return(i.contents)
        }
        i.contents < 3
      } {
        i.contents = i.contents + 1
        continue
      }
      99
    }
    eq(__LOC__, 2, f(2))
    eq(__LOC__, 99, f(5))
    let outerBreak = early => {
      let i = ref(0)
      while i.contents < 5 {
        i.contents = i.contents + 1
        while {
          if i.contents == 2 {
            break
          }
          if early {
            %return(42)
          }
          true
        } {
          break
        }
      }
      i.contents
    }
    eq(__LOC__, 2, outerBreak(false))
    eq(__LOC__, 42, outerBreak(true))
  })

  test("return is not caught but operand exceptions are", () => {
    eq(__LOC__, 42, inTry(false))
    eq(__LOC__, 7, inTry(true))
  })

  test("expression position preserves evaluation and skips later effects", () => {
    let seen = ref(list{})
    let effect = n => {
      seen.contents = list{n, ...seen.contents}
      n
    }
    let f = early => {
      let first = effect(1)
      let pair = (
        first,
        if early {
          %return(effect(2))
        } else {
          effect(3)
        },
      )
      ignore(effect(4))
      let (a, b) = pair
      a + b
    }
    eq(__LOC__, 2, f(true))
    eq(__LOC__, list{2, 1}, seen.contents)
    seen.contents = list{}
    eq(__LOC__, 4, f(false))
    eq(__LOC__, list{4, 3, 1}, seen.contents)
  })

  test("return from for-of closes the iterator", () => {
    let closed = ref(false)
    let values: (unit => unit) => iterable<int> = %raw(`
      onClose => (function* () {
        try { yield 1; yield 2; }
        finally { onClose(); }
      })()
    `)
    let f = () => {
      for value of values(() => {closed.contents = true}) {
        %return(value)
      }
      0
    }
    eq(__LOC__, 1, f())
    eq(__LOC__, true, closed.contents)
  })

  test("short circuit and unit return", () => {
    let seen = ref(0)
    let f = enabled => {
      ignore(enabled && {%return(42)})
      7
    }
    let stop = () => {
      if true {
        %return(())
      }
      seen.contents = 1
    }
    stop()
    eq(__LOC__, 0, seen.contents)
    eq(__LOC__, 42, f(true))
    eq(__LOC__, 7, f(false))
  })

  testAsync("async returns", async () => {
    eq(__LOC__, 42, await returnAsync(true))
    eq(__LOC__, 7, await returnAsync(false))
  })
})
