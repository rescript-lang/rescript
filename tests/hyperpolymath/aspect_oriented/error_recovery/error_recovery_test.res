// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Error recovery
// Cross-cutting concern: exception handling, error propagation, and recovery
// patterns across all compiler phases.

let eq = (a, b) => a == b

// ─── Basic try/catch recovery ───────────────────────────────────────
exception RecoveryError(string)

let recover = f =>
  try {
    Ok(f())
  } catch {
  | RecoveryError(msg) => Error(msg)
  | exn => Error(Printexc.to_string(exn))
  }

Test.run(__POS_OF__("recovery: success"), recover(() => 42), eq, Ok(42))
Test.run(
  __POS_OF__("recovery: caught"),
  recover(() => raise(RecoveryError("oops"))),
  eq,
  Error("oops"),
)

// ─── Exception chaining ────────────────────────────────────────────
exception Level1Error(string)
exception Level2Error(string, string)

let level2 = () => raise(Level1Error("original"))

let level1 = () =>
  try {
    level2()
  } catch {
  | Level1Error(msg) => raise(Level2Error("wrapped", msg))
  }

let chainResult =
  try {
    level1()
  } catch {
  | Level2Error(wrapper, original) => `${wrapper}: ${original}`
  | _ => "unknown"
  }
Test.run(__POS_OF__("recovery: chain"), chainResult, eq, "wrapped: original")

// ─── Custom exception types with payloads ───────────────────────────
exception HttpError({code: int, message: string, url: string})

let httpResult =
  try {
    raise(HttpError({code: 404, message: "Not Found", url: "/api/data"}))
  } catch {
  | HttpError({code, message, url}) =>
    `${Int.toString(code)} ${message} at ${url}`
  }
Test.run(__POS_OF__("recovery: inline record exn"), httpResult, eq, "404 Not Found at /api/data")

// ─── Nested try/catch ───────────────────────────────────────────────
exception InnerError
exception OuterError

let nestedResult = {
  try {
    try {
      raise(InnerError)
    } catch {
    | InnerError => "inner caught"
    }
  } catch {
  | OuterError => "outer caught"
  | _ => "unexpected"
  }
}
Test.run(__POS_OF__("recovery: nested inner"), nestedResult, eq, "inner caught")

// Outer catch catches what inner doesn't
let outerCatchResult = {
  try {
    try {
      raise(OuterError)
    } catch {
    | InnerError => "inner"
    }
  } catch {
  | OuterError => "outer"
  | _ => "unknown"
  }
}
Test.run(__POS_OF__("recovery: nested outer"), outerCatchResult, eq, "outer")

// ─── Exception in callbacks ─────────────────────────────────────────
let safeMap = (arr, f) =>
  arr->Array.map(x =>
    try {
      Ok(f(x))
    } catch {
    | exn => Error(Printexc.to_string(exn))
    }
  )

exception DivByZero

let results = safeMap([4, 2, 0, 6], x =>
  if x == 0 {
    raise(DivByZero)
  } else {
    10 / x
  }
)

Test.run(__POS_OF__("recovery: callback ok"), Result.isOk(results[0]), eq, true)
Test.run(__POS_OF__("recovery: callback err"), Result.isError(results[2]), eq, true)
Test.run(
  __POS_OF__("recovery: callback value"),
  Result.getOr(results[0], 0),
  eq,
  2,
)

// ─── Exception in pattern matching ──────────────────────────────────
exception InvalidInput(string)

type input = Valid(int) | Invalid(string)

let processInput = inp =>
  try {
    switch inp {
    | Valid(n) if n < 0 => raise(InvalidInput("negative"))
    | Valid(n) => Ok(n * 2)
    | Invalid(msg) => raise(InvalidInput(msg))
    }
  } catch {
  | InvalidInput(msg) => Error(msg)
  }

Test.run(__POS_OF__("recovery: match valid"), processInput(Valid(5)), eq, Ok(10))
Test.run(
  __POS_OF__("recovery: match negative"),
  processInput(Valid(-1)),
  eq,
  Error("negative"),
)
Test.run(
  __POS_OF__("recovery: match invalid"),
  processInput(Invalid("bad")),
  eq,
  Error("bad"),
)

// ─── Re-raising exceptions ──────────────────────────────────────────
exception Rethrowed

let reraise = () => {
  try {
    try {
      raise(Rethrowed)
    } catch {
    | Rethrowed as e =>
      // Log and re-raise
      raise(e)
    }
  } catch {
  | Rethrowed => "re-caught"
  }
}
Test.run(__POS_OF__("recovery: reraise"), reraise(), eq, "re-caught")

// ─── Error boundary pattern ─────────────────────────────────────────
let errorBoundary = (f, fallback) =>
  try {
    f()
  } catch {
  | _ => fallback
  }

Test.run(
  __POS_OF__("recovery: boundary ok"),
  errorBoundary(() => 42, 0),
  eq,
  42,
)
Test.run(
  __POS_OF__("recovery: boundary fail"),
  errorBoundary(() => raise(Not_found), 0),
  eq,
  0,
)

// ─── Multiple catch clauses via pattern matching on exn ─────────────
exception TypeError(string)
exception ValueError(string)
exception RangeError(string)

let multiCatch = f =>
  try {
    f()
  } catch {
  | TypeError(msg) => `type: ${msg}`
  | ValueError(msg) => `value: ${msg}`
  | RangeError(msg) => `range: ${msg}`
  | _ => "unknown error"
  }

Test.run(
  __POS_OF__("recovery: multi type"),
  multiCatch(() => raise(TypeError("int expected"))),
  eq,
  "type: int expected",
)
Test.run(
  __POS_OF__("recovery: multi value"),
  multiCatch(() => raise(ValueError("invalid"))),
  eq,
  "value: invalid",
)
Test.run(
  __POS_OF__("recovery: multi range"),
  multiCatch(() => raise(RangeError("out of bounds"))),
  eq,
  "range: out of bounds",
)
Test.run(
  __POS_OF__("recovery: multi unknown"),
  multiCatch(() => raise(Not_found)),
  eq,
  "unknown error",
)

// ─── Result-based error handling pipeline ───────────────────────────
let parseStep = (s): result<int, string> =>
  switch Int.fromString(s) {
  | Some(n) => Ok(n)
  | None => Error(`parse error: ${s}`)
  }

let validateStep = (n): result<int, string> =>
  if n >= 0 && n <= 100 {
    Ok(n)
  } else {
    Error(`out of range: ${Int.toString(n)}`)
  }

let transformStep = (n): result<string, string> => Ok(`result: ${Int.toString(n * 2)}`)

let pipeline = s =>
  parseStep(s)
  ->Result.flatMap(validateStep)
  ->Result.flatMap(transformStep)

Test.run(__POS_OF__("recovery: pipeline ok"), pipeline("42"), eq, Ok("result: 84"))
Test.run(
  __POS_OF__("recovery: pipeline parse err"),
  pipeline("abc"),
  eq,
  Error("parse error: abc"),
)
Test.run(
  __POS_OF__("recovery: pipeline range err"),
  pipeline("200"),
  eq,
  Error("out of range: 200"),
)

// ─── Error message preservation ─────────────────────────────────────
let jsError = Error.make("test error message")
Test.run(__POS_OF__("recovery: Error.message"), Error.message(jsError), eq, "test error message")

// ─── Stack trace availability ───────────────────────────────────────
let hasStack = {
  let err = Error.make("stack test")
  switch Error.stack(err) {
  | Some(s) => String.length(s) > 0
  | None => false
  }
}
Test.run(__POS_OF__("recovery: stack available"), hasStack, eq, true)
