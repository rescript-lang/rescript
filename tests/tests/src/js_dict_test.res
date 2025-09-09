open Js_dict
open Mocha
open Test_utils

let obj = (): t<'a> => Obj.magic({"foo": 43, "bar": 86})

describe(__MODULE__, () => {
  test("empty", () => {
    eq(__LOC__, [], keys(empty()))
  })
  test("get", () => {
    eq(__LOC__, Some(43), get(obj(), "foo"))
  })
  test("get - property not in object", () => {
    eq(__LOC__, None, get(obj(), "baz"))
  })
  test("unsafe_get", () => {
    eq(__LOC__, 43, unsafeGet(obj(), "foo"))
  })
  test("set", () => {
    let o = obj()
    set(o, "foo", 36)
    eq(__LOC__, Some(36), get(o, "foo"))
  })
  test("keys", () => {
    eq(__LOC__, ["foo", "bar"], keys(obj()))
  })
  test("entries", () => {
    eq(__LOC__, [("foo", 43), ("bar", 86)], entries(obj()))
  })
  test("values", () => {
    eq(__LOC__, [43, 86], values(obj()))
  })
  test("fromList - []", () => {
    eq(__LOC__, empty(), fromList(list{}))
  })
  test("fromList", () => {
    eq(__LOC__, [("x", 23), ("y", 46)], entries(fromList(list{("x", 23), ("y", 46)})))
  })
  test("fromArray - []", () => {
    eq(__LOC__, empty(), fromArray([]))
  })
  test("fromArray", () => {
    eq(__LOC__, [("x", 23), ("y", 46)], entries(fromArray([("x", 23), ("y", 46)])))
  })
  test("map", () => {
    eq(__LOC__, Obj.magic({"foo": "43", "bar": "86"}), map(i => Js.Int.toString(i), obj()))
  })
})
