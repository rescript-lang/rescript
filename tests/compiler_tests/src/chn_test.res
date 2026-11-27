open Mocha
open Test_utils

Console.log(`你好，
世界`)

Console.log(`\x3f\u003f\b\t\n\v\f\r\0"'`)

let convert = (s: string): list<int> =>
  List.fromArray(
    s
    ->String.asIterable
    ->Array.fromIterable
    ->Array.map(x =>
      switch String.codePointAt(x, 0) {
      | None => assert(false)
      | Some(x) => x
      }
    ),
  )

describe(__MODULE__, () => {
  test("Chinese string newline", () =>
    eq(
      __LOC__,
      `你好，
世界`,
      `你好，\n世界`,
    )
  )

  test("Convert Chinese characters", () =>
    eq(
      __LOC__,
      convert(`汉字是世界上最美丽的character`),
      list{
        27721,
        23383,
        26159,
        19990,
        30028,
        19978,
        26368,
        32654,
        20029,
        30340,
        99,
        104,
        97,
        114,
        97,
        99,
        116,
        101,
        114,
      },
    )
  )

  test("Convert hex escape", () => eq(__LOC__, convert(`\x3f\x3fa`), list{63, 63, 97}))
  test("Convert question marks", () => eq(__LOC__, convert(`??a`), list{63, 63, 97}))
  test("Convert unicode escape", () => eq(__LOC__, convert(`\u003f\x3fa`), list{63, 63, 97}))
  test("Convert rocket emoji with a", () =>
    eq(__LOC__, convert(`🚀🚀a`), list{128640, 128640, 97})
  )
  test("Convert rocket emoji surrogate with a", () =>
    eq(__LOC__, convert(`\uD83D\uDE80a`), list{128640, 97})
  )
  test("Convert rocket emoji surrogate with question", () =>
    eq(__LOC__, convert(`\uD83D\uDE80\x3f`), list{128640, 63})
  )

  test("Convert double rocket emoji with a", () =>
    eq(__LOC__, convert(`\uD83D\uDE80\uD83D\uDE80a`), list{128640, 128640, 97})
  )

  test("String length with emoji", () => eq(__LOC__, String.length(`\uD83D\uDE80\0`), 3))

  test("String get emoji with null", () =>
    eq(__LOC__, String.codePointAt(`\uD83D\uDE80\0`, 0), Some(128640))
  )
  test("String get rocket emoji", () => eq(__LOC__, String.codePointAt(`🚀`, 0), Some(128640)))

  test("Convert rocket emoji", () => eq(__LOC__, convert(`\uD83D\uDE80`), list{128640}))
  test("Convert double rocket emoji", () =>
    eq(__LOC__, convert(`\uD83D\uDE80\uD83D\uDE80`), list{128640, 128640})
  )
  test("Convert whitespace chars", () =>
    eq(__LOC__, convert(` \b\t\n\v\f\ra`), list{32, 8, 9, 10, 11, 12, 13, 97})
  )
  test("Convert escaped chars", () =>
    eq(__LOC__, convert(` \b\t\n\v\f\r"'\\\0a`), list{32, 8, 9, 10, 11, 12, 13, 34, 39, 92, 0, 97})
  )
})
