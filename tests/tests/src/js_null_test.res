open Js_null
open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("toOption - empty", () => {
    eq(__LOC__, None, toOption(empty))
  })

  test("toOption - 'a", () => {
    eq(__LOC__, Some(), toOption(return()))
  })

  test("return", () => {
    eq(__LOC__, Some("something"), toOption(return("something")))
  })

  test("test - empty", () => {
    eq(__LOC__, true, empty == Js.null)
  })

  test("test - 'a", () => {
    eq(__LOC__, false, return() == empty)
  })

  test("bind - empty", () => {
    eq(__LOC__, empty, bind(empty, v => v))
  })

  test("bind - 'a", () => {
    eq(__LOC__, return(4), bind(return(2), n => n * 2))
  })

  test("iter - empty", () => {
    let hit = ref(false)
    let _ = iter(empty, _ => hit := true)
    eq(__LOC__, false, hit.contents)
  })

  test("iter - 'a", () => {
    let hit = ref(0)
    let _ = iter(return(2), v => hit := v)
    eq(__LOC__, 2, hit.contents)
  })

  test("fromOption - None", () => {
    eq(__LOC__, empty, fromOption(None))
  })

  test("fromOption - Some", () => {
    eq(__LOC__, return(2), fromOption(Some(2)))
  })
})
