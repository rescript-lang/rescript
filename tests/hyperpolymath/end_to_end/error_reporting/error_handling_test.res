// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: Error handling and reporting
// Tests runtime error handling, exception messages, and error recovery.

let eq = (a, b) => a == b

// ─── Exception messages ─────────────────────────────────────────────
exception AppError(string)
exception CodedError({code: int, message: string})

let catchMessage = f =>
  try {
    f()
    None
  } catch {
  | AppError(msg) => Some(msg)
  | CodedError({message}) => Some(message)
  | _ => Some("unknown")
  }

Test.run(
  __POS_OF__("error: custom exception message"),
  catchMessage(() => raise(AppError("something failed"))),
  eq,
  Some("something failed"),
)
Test.run(
  __POS_OF__("error: coded exception"),
  catchMessage(() => raise(CodedError({code: 500, message: "internal error"}))),
  eq,
  Some("internal error"),
)
Test.run(
  __POS_OF__("error: no throw"),
  catchMessage(() => ()),
  eq,
  None,
)

// ─── Invalid JSON parsing ──────────────────────────────────────────
let parseJson = s =>
  try {
    Ok(JSON.parseExn(s))
  } catch {
  | _ => Error("parse failed")
  }

Test.run(__POS_OF__("error: json valid"), Result.isOk(parseJson(`{"a": 1}`)), eq, true)
Test.run(__POS_OF__("error: json invalid"), parseJson("{{invalid}}"), eq, Error("parse failed"))
Test.run(__POS_OF__("error: json empty"), parseJson(""), eq, Error("parse failed"))

// ─── Type assertion failures ────────────────────────────────────────
let assertResult =
  try {
    assert(true)
    "passed"
  } catch {
  | _ => "failed"
  }
Test.run(__POS_OF__("error: assert true"), assertResult, eq, "passed")

let assertFail =
  try {
    assert(false)
    "passed"
  } catch {
  | _ => "failed"
  }
Test.run(__POS_OF__("error: assert false"), assertFail, eq, "failed")

// ─── Custom error types ────────────────────────────────────────────
type errorKind = NotFound | Unauthorized | Forbidden | Timeout | ServerError(int)

exception ApiError(errorKind)

let handleApiError = f =>
  try {
    Ok(f())
  } catch {
  | ApiError(NotFound) => Error("404: not found")
  | ApiError(Unauthorized) => Error("401: unauthorized")
  | ApiError(Forbidden) => Error("403: forbidden")
  | ApiError(Timeout) => Error("408: timeout")
  | ApiError(ServerError(code)) => Error(`${Int.toString(code)}: server error`)
  | _ => Error("unknown error")
  }

Test.run(
  __POS_OF__("error: api not found"),
  handleApiError(() => raise(ApiError(NotFound))),
  eq,
  Error("404: not found"),
)
Test.run(
  __POS_OF__("error: api server error"),
  handleApiError(() => raise(ApiError(ServerError(503)))),
  eq,
  Error("503: server error"),
)
Test.run(
  __POS_OF__("error: api success"),
  handleApiError(() => 42),
  eq,
  Ok(42),
)

// ─── Error recovery with default values ─────────────────────────────
let safeDiv = (a, b) =>
  try {
    if b == 0 {
      raise(Division_by_zero)
    }
    Ok(a / b)
  } catch {
  | Division_by_zero => Error("division by zero")
  }

Test.run(__POS_OF__("error: safe div ok"), safeDiv(10, 3), eq, Ok(3))
Test.run(
  __POS_OF__("error: safe div zero"),
  safeDiv(10, 0),
  eq,
  Error("division by zero"),
)

// ─── Error accumulation pattern ─────────────────────────────────────
let validateAll = (validators, value) => {
  let errors = validators->Array.filterMap(validate =>
    switch validate(value) {
    | Ok(_) => None
    | Error(msg) => Some(msg)
    }
  )
  if Array.length(errors) == 0 {
    Ok(value)
  } else {
    Error(errors)
  }
}

let validators = [
  n =>
    if n >= 0 {
      Ok(n)
    } else {
      Error("must be non-negative")
    },
  n =>
    if n <= 100 {
      Ok(n)
    } else {
      Error("must be at most 100")
    },
  n =>
    if n mod 2 == 0 {
      Ok(n)
    } else {
      Error("must be even")
    },
]

Test.run(
  __POS_OF__("error: accumulation valid"),
  validateAll(validators, 42),
  eq,
  Ok(42),
)
Test.run(
  __POS_OF__("error: accumulation 1 error"),
  validateAll(validators, 43),
  eq,
  Error(["must be even"]),
)
Test.run(
  __POS_OF__("error: accumulation 2 errors"),
  validateAll(validators, -3),
  eq,
  Error(["must be non-negative", "must be even"]),
)

// ─── Retry pattern ─────────────────────────────────────────────────
exception TransientError

let withRetry = (~maxRetries=3, f) => {
  let attempt = ref(0)
  let result = ref(Error("max retries exceeded"))
  while attempt.contents < maxRetries && Result.isError(result.contents) {
    attempt := attempt.contents + 1
    result :=
      try {
        Ok(f(attempt.contents))
      } catch {
      | TransientError => Error(`attempt ${Int.toString(attempt.contents)} failed`)
      }
  }
  result.contents
}

// Succeeds on third attempt
let retryResult = withRetry(n =>
  if n < 3 {
    raise(TransientError)
  } else {
    "success"
  }
)
Test.run(__POS_OF__("error: retry succeeds"), retryResult, eq, Ok("success"))

// All attempts fail
let retryFail = withRetry(_ => raise(TransientError))
Test.run(__POS_OF__("error: retry fails"), Result.isError(retryFail), eq, true)

// ─── Error.make and properties ──────────────────────────────────────
let err = Error.make("test error")
Test.run(__POS_OF__("error: Error.message"), Error.message(err), eq, "test error")

let hasStack = switch Error.stack(err) {
| Some(s) => String.length(s) > 0
| None => false
}
Test.run(__POS_OF__("error: Error.stack"), hasStack, eq, true)

// ─── Finally-like pattern ──────────────────────────────────────────
let cleanup = ref(false)

let withCleanup = f => {
  let result = try {
    Ok(f())
  } catch {
  | exn => Error(exn)
  }
  cleanup := true // always runs
  result
}

let _ = withCleanup(() => 42)
Test.run(__POS_OF__("error: cleanup on success"), cleanup.contents, eq, true)

cleanup := false
let _ = withCleanup(() => raise(Not_found))
Test.run(__POS_OF__("error: cleanup on failure"), cleanup.contents, eq, true)
