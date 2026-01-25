open Js_global
open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("setTimeout/clearTimeout sanity check", () => {
    let handle = setTimeout(() => (), 0)
    clearTimeout(handle)
    eq(__LOC__, true, true)
  })
  test("setInterval/clearInterval sanity check", () => {
    let handle = setInterval(() => (), 0)
    clearInterval(handle)
    eq(__LOC__, true, true)
  })
  test("encodeURI", () => {
    eq(__LOC__, "%5B-=-%5D", encodeURI("[-=-]"))
  })
  test("decodeURI", () => {
    eq(__LOC__, "[-=-]", decodeURI("%5B-=-%5D"))
  })
  test("encodeURIComponent", () => {
    eq(__LOC__, "%5B-%3D-%5D", encodeURIComponent("[-=-]"))
  })
  test("decodeURIComponent", () => {
    eq(__LOC__, "[-=-]", decodeURIComponent("%5B-%3D-%5D"))
  })
})
