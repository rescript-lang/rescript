let eq = (a, b) => a == b

let someString = "hello"

let createdDict = dict{
  "name": "hello",
  "age": "what",
  "more": "stuff",
  "otherStr": someString,
}

let three = 3

let intDict = dict{
  "one": 1,
  "two": 2,
  "three": three,
}

module PatternMatching = {
  let inferDictByPattern = dict =>
    switch dict {
    | dict{"one": 1, "three": 3, "four": 4} =>
      // Make sure that the dict is of correct type
      dict->Js.Dict.set("five", 5)
    | dict{"two": 1} => Js.log("two")
    | _ => Js.log("not one")
    }

  let constrainedAsDict = (dict: dict<int>) =>
    switch dict {
    | dict{"one": 1} =>
      let _d: dict<int> = dict
      Js.log("one")
    | _ => Js.log("not one")
    }
}

Test.run(__POS_OF__("make"), Dict.make(), eq, %raw(`{}`))

Test.run(__POS_OF__("fromArray"), Dict.fromArray([("foo", "bar")]), eq, %raw(`{foo: "bar"}`))

Test.run(
  __POS_OF__("getUnsafe - existing"),
  Dict.fromArray([("foo", "bar")])->Dict.getUnsafe("foo"),
  eq,
  "bar",
)
Test.run(
  __POS_OF__("getUnsafe - missing"),
  Dict.make()->Dict.getUnsafe("foo"),
  eq,
  %raw(`undefined`),
)

module Has = {
  let dict = dict{
    "key1": Some(false),
    "key2": None,
  }

  Test.run(__POS_OF__("has - existing"), dict->Dict.has("key1"), eq, true)
  Test.run(__POS_OF__("has - existing None"), dict->Dict.has("key2"), eq, true)
  Test.run(__POS_OF__("has - missing"), dict->Dict.has("key3"), eq, false)
  Test.run(__POS_OF__("has - prototype"), dict->Dict.has("toString"), eq, true)
  Test.run(
    __POS_OF__("has - parantesis in generated code"),
    typeof(dict->Dict.has("key1")),
    eq,
    #boolean,
  )
}
