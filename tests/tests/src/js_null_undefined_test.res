open Js_null_undefined
open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("toOption - null", () => {
    eq(__LOC__, None, toOption(null))
  })
  test("toOption - undefined", () => {
    eq(__LOC__, None, toOption(undefined))
  })
  test("toOption - empty", () => {
    eq(__LOC__, None, toOption(undefined))
  })
  test("toOption - return", () => {
    eq(__LOC__, Some("foo"), toOption(return("foo")))
  })
  test("return", () => {
    eq(__LOC__, Some("something"), toOption(return("something")))
  })
  test("test - null", () => {
    eq(__LOC__, true, isNullable(null))
  })
  test("test - undefined", () => {
    eq(__LOC__, true, isNullable(undefined))
  })
  test("test - empty", () => {
    eq(__LOC__, true, isNullable(undefined))
  })
  test("test - return", () => {
    eq(__LOC__, true, isNullable(return()))
  })
  test("bind - null", () => {
    eq(__LOC__, null, bind(null, v => v))
  })
  test("bind - undefined", () => {
    eq(__LOC__, undefined, bind(undefined, v => v))
  })
  test("bind - empty", () => {
    eq(__LOC__, undefined, bind(undefined, v => v))
  })
  test("bind - 'a", () => {
    eq(__LOC__, return(4), bind(return(2), n => n * 2))
  })
  test("iter - null", () => {
    let hit = ref(false)
    let _ = iter(null, _ => hit := true)
    eq(__LOC__, false, hit.contents)
  })
  test("iter - undefined", () => {
    let hit = ref(false)
    let _ = iter(undefined, _ => hit := true)
    eq(__LOC__, false, hit.contents)
  })
  test("iter - empty", () => {
    let hit = ref(false)
    let _ = iter(undefined, _ => hit := true)
    eq(__LOC__, false, hit.contents)
  })
  test("iter - 'a", () => {
    let hit = ref(0)
    let _ = iter(return(2), v => hit := v)
    eq(__LOC__, 2, hit.contents)
  })
  test("fromOption - None", () => {
    eq(__LOC__, undefined, fromOption(None))
  })
  test("fromOption - Some", () => {
    eq(__LOC__, return(2), fromOption(Some(2)))
  })
  test("null <> undefined", () => {
    eq(__LOC__, true, null != undefined)
  })
  test("null <> empty", () => {
    eq(__LOC__, true, null != undefined)
  })
  test("undefined = empty", () => {
    eq(__LOC__, true, undefined == undefined)
  })
  test("null variable", () => {
    let null = 3
    eq(__LOC__, true, !Js.isNullable(Js.Nullable.return(null)))
  })
})
