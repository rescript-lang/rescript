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
    | dict{"two": 1} => Console.log("two")
    | _ => Console.log("not one")
    }

  let constrainedAsDict = (dict: dict<int>) =>
    switch dict {
    | dict{"one": 1} =>
      let _d: dict<int> = dict
      Console.log("one")
    | _ => Console.log("not one")
    }
}

Test.run(__POS_OF__("make"), Dict.make(), eq, %raw(`{}`))

Test.run(__POS_OF__("fromArray"), Dict.fromArray([("foo", "bar")]), eq, %raw(`{foo: "bar"}`))

Test.run(
  __POS_OF__("fromIterable"),
  Dict.fromIterable([("foo", "bar"), ("baz", "qux")]->Array.asIterable),
  eq,
  %raw(`{foo: "bar", baz: "qux"}`),
)

{
  let target = dict{
    "a": 1,
    "b": 2,
  }
  let result = target->Dict.assignMany([dict{"b": 3}, dict{"b": 4, "c": 0}])

  Test.run(
    __POS_OF__("assignMany copies from sources to target"),
    result,
    eq,
    dict{
      "a": 1,
      "b": 4,
      "c": 0,
    },
  )
  Test.run(__POS_OF__("assignMany mutates target"), result === target, eq, true)
}

{
  let target = dict{
    "a": 1,
    "b": 2,
  }
  let result = target->Dict.concat(dict{"b": 3, "c": 0})

  Test.run(
    __POS_OF__("concat copies into a fresh dictionary"),
    result,
    eq,
    dict{
      "a": 1,
      "b": 3,
      "c": 0,
    },
  )
  Test.run(
    __POS_OF__("concat leaves target unchanged"),
    target,
    eq,
    dict{
      "a": 1,
      "b": 2,
    },
  )
  Test.run(__POS_OF__("concat returns a fresh dictionary"), result === target, eq, false)
}

{
  let target = dict{
    "a": 1,
    "b": 2,
  }
  let result = target->Dict.concatMany([dict{"b": 3}, dict{"b": 4, "c": 0}])

  Test.run(
    __POS_OF__("concatMany copies into a fresh dictionary"),
    result,
    eq,
    dict{
      "a": 1,
      "b": 4,
      "c": 0,
    },
  )
  Test.run(
    __POS_OF__("concatMany leaves target unchanged"),
    target,
    eq,
    dict{
      "a": 1,
      "b": 2,
    },
  )
  Test.run(__POS_OF__("concatMany returns a fresh dictionary"), result === target, eq, false)
}

{
  let first = dict{
    "a": 1,
    "b": 2,
  }
  let last = dict{
    "b": 4,
    "c": 0,
  }
  let result = Dict.concatAll([first, dict{"b": 3}, last])

  Test.run(
    __POS_OF__("concatAll copies into a fresh dictionary"),
    result,
    eq,
    dict{
      "a": 1,
      "b": 4,
      "c": 0,
    },
  )
  Test.run(
    __POS_OF__("concatAll leaves first source unchanged"),
    first,
    eq,
    dict{
      "a": 1,
      "b": 2,
    },
  )
  Test.run(
    __POS_OF__("concatAll leaves later source unchanged"),
    last,
    eq,
    dict{
      "b": 4,
      "c": 0,
    },
  )
  Test.run(__POS_OF__("concatAll returns a fresh dictionary"), result === first, eq, false)
}

Test.run(
  __POS_OF__("concatAll with empty array returns empty dictionary"),
  Dict.concatAll([]),
  eq,
  dict{},
)

{
  let foo = dict{
    "a": 1,
    "b": 2,
  }
  let baz = dict{
    "b": 4,
    "c": 5,
  }
  let result = dict{...foo, "b": 3, ...baz, "d": 6}

  Test.run(
    __POS_OF__("dict spread respects overwrite order"),
    result,
    eq,
    dict{
      "a": 1,
      "b": 4,
      "c": 5,
      "d": 6,
    },
  )
  Test.run(
    __POS_OF__("dict spread leaves first source unchanged"),
    foo,
    eq,
    dict{
      "a": 1,
      "b": 2,
    },
  )
  Test.run(
    __POS_OF__("dict spread leaves later source unchanged"),
    baz,
    eq,
    dict{
      "b": 4,
      "c": 5,
    },
  )
}

{
  let foo = dict{"a": 1}
  let result = dict{...foo}

  Test.run(__POS_OF__("dict spread clone copies values"), result, eq, foo)
  Test.run(__POS_OF__("dict spread clone returns a fresh dictionary"), result === foo, eq, false)
}

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
