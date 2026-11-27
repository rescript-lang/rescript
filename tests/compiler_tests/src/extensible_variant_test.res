open Mocha
open Test_utils

type attr = ..

type attr += Str(string)

module N = {
  type attr += Int(int, int)
}

type attr += Int(int, int)

let to_int = (x: attr) =>
  switch x {
  | Str(_) => -1
  | N.Int(a, _) => a
  | Int(_, b) => b
  | _ => assert(false)
  }

// Inline records keep their field names, including @as renames.
type attr += Point({@as("x-coord") x: int, y: int})

// Rebinding gives an existing constructor a second name.
type attr += StrAlias = Str

// Extensible types can be indexed like GADTs.
type key<_> = ..
type key<_> += IntKey: key<int>
type key<_> += StringKey: key<string>

let defaultOf: type a. key<a> => option<a> = key =>
  switch key {
  | IntKey => Some(0)
  | StringKey => Some("")
  | _ => None
  }

// Outside code can match a private extensible type but not extend it.
module Private: {
  type t = private ..
  type t += A
  let b: t
} = {
  type t = ..
  type t += A | B
  let b = B
}

let privateName = (t: Private.t) =>
  switch t {
  | Private.A => "A"
  | _ => "other"
  }

// A local exception is a new constructor on every call.
let makeException = () => {
  exception Local
  let raise = () => throw(Local)
  let isLocal = exn =>
    switch exn {
    | Local => true
    | _ => false
    }
  (raise, isLocal)
}

describe(__MODULE__, () => {
  test("test_int", () => {
    eq(__LOC__, 3, to_int(N.Int(3, 0)))
  })

  test("test_int2", () => {
    eq(__LOC__, 0, to_int(Int(3, 0)))
  })

  test("test_string", () => {
    eq(__LOC__, -1, to_int(Str("x")))
  })

  test("inline record", () => {
    let point = Point({x: 1, y: 2})
    eq(__LOC__, 1, (Obj.magic(point): {"x-coord": int})["x-coord"])
    eq(
      __LOC__,
      3,
      switch point {
      | Point({x, y}) => x + y
      | _ => 0
      },
    )
  })

  test("rebind", () => {
    eq(__LOC__, -1, to_int(StrAlias("x")))
    eq(__LOC__, true, StrAlias("x") == Str("x"))
  })

  test("indexed extension", () => {
    eq(__LOC__, Some(0), defaultOf(IntKey))
    eq(__LOC__, Some(""), defaultOf(StringKey))
  })

  test("private extensible", () => {
    eq(__LOC__, "A", privateName(Private.A))
    eq(__LOC__, "other", privateName(Private.b))
  })

  test("local exceptions are generative", () => {
    let (raise1, isLocal1) = makeException()
    let (_, isLocal2) = makeException()
    let caught = try raise1() catch {
    | exn => Some(exn)
    }
    switch caught {
    | Some(exn) =>
      eq(__LOC__, true, isLocal1(exn))
      eq(__LOC__, false, isLocal2(exn))
    | None => ok(__LOC__, false)
    }
  })
})
