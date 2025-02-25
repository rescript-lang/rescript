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

module DictHas = {
  let dict = dict{
    "key1": Some(1),
    "key2": None,
  }

  assert(dict->Dict.has("key1") === true)
  assert(dict->Dict.has("key2") === true)
  assert(dict->Dict.has("key3") === false)
}
