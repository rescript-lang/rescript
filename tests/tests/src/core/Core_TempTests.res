include Core_IntlTests

Console.info("")
Console.info("Array")
Console.info("---")
let array = [1, 2, 3, 4]
Console.info(array->Array.map(x => x * 2)->Array.reduce(0, (a, b) => a + b))
Console.info(typeof(array))

Console.info("")
Console.info("Date")
Console.info("---")
let date = Date.fromTime(Date.UTC.makeWithYM(~year=2020, ~month=11))
Console.log(date->Date.toUTCString)

Console.info("")
Console.info("Dict")
Console.info("---")
let dict = Dict.make()
dict->Dict.set("foo", "bar")
let dict2 = dict->Dict.copy
dict2->Dict.delete("foo")
Console.log2(dict, dict2)

Console.info("")
Console.info("Error")
Console.info("---")
let f = () => {
  let error = Error.make("hello")
  let typeError = Error.TypeError.make("error")
  let g = () => Error.throw(error)
  let h = () => Error.throw(typeError)
  (g, h)
}

Console.info("")
Console.info("Float/Int")
Console.info("---")
Console.log(10.2->Float.toFixed(~digits=2))
Console.log(10->Int.toFixed(~digits=2))
Console.log("0"->Int.fromString)
Console.log("0.1"->Float.fromString)

Console.info("")
Console.info("JSON")
Console.info("---")
let json = JSON.parseExn(`{"foo": "bar"}`)
Console.log(
  switch JSON.Classify.classify(json) {
  | Object(json) =>
    switch JSON.Classify.classify(json->Dict.get("foo")) {
    | String(value) => Some(value)
    | _ => None
    }
  | _ => None
  },
)

Console.info("")
Console.info("Map")
Console.info("---")
let map = Map.make()
let _ = map->Map.set(1, 1)
let _ = map->Map.set(2, 2)
let _ = map->Map.delete(1)
Console.log(map)

Console.info("")
Console.info("Math")
Console.info("---")
Console.log(Math.ceil(Math.Constants.pi /. 2.0))

Console.info("")
Console.info("BigInt")
Console.info("---")

@warning("-44")
Console.log({
  open BigInt
  fromInt(1) / fromFloat(12.0)->Option.getOr(0n)
})

Console.info("")
Console.info("Object")
Console.info("---")
let myObject = {
  "foo": "bar",
}

Console.log(Object.create(myObject))
Console.log(Object.createWithProperties(myObject, {"foo": {"value": "bar"}}))
Console.log(Object.createWithNull())
Console.log(Object.createWithNullAndProperties({"foo": {"value": "bar"}}))

let copy = Object.copy(myObject)
let withNewProp = Object.assign(copy, {"bar": "baz"})

Console.info("")
Console.info("Promise")
Console.info("---")
let promise: promise<int> = Promise.make((resolve, _reject) => {
  let _ = setTimeout(() => {
    resolve(1)
  }, 100)
})

let _ =
  promise
  ->Promise.then(x => Promise.resolve(x + 1))
  ->Promise.then(x => Promise.resolve(x + 2))
  ->Promise.then(x => {
    Console.log(x)
    Promise.resolve()
  })
  ->Promise.finally(() => {
    Console.log("Promise finally")
  })

Console.info("")
Console.info("RegExp")
Console.info("---")
let regex = RegExp.fromString("hello(\\w+)")
let string = "helloworld"
Console.log(regex->RegExp.test(string))
let result = regex->RegExp.exec(string)
Console.log(result->Option.map(RegExp.Result.input))
Console.log(result->Option.map(RegExp.Result.index))
Console.log(result->Option.map(RegExp.Result.matches))

Console.info("")
Console.info("Set")
Console.info("---")
let set = Set.make()
set->Set.add(1)
set->Set.add(2)
let _ = set->Set.delete(2)
Console.log(set)

Console.info("")
Console.info("String")
Console.info("---")
let regexp = RegExp.fromString("(hello )(world)")
Console.log(
  "   Hello world  "
  ->String.toLowerCase
  ->String.trim
  ->String.unsafeReplaceRegExpBy2(regexp, (
    ~match as _,
    ~group1,
    ~group2,
    ~offset as _,
    ~input as _,
  ) => {
    group1 ++ group2->String.toUpperCase
  }),
)

Console.info("")
Console.info("Symbol")
Console.info("---")
let x = Symbol.getFor("Foo")
Console.log(x)
let array: array<string> = String.getSymbolUnsafe("foo", Symbol.iterator)()->Iterator.toArray
Console.log(array)

Console.info("")
Console.info("Global namespace")
Console.info("---")

Console.log(typeof(1))

let timeout = setTimeout(() => {
  Console.log("Hello!")
}, 100)

clearTimeout(timeout)

if globalThis["hello"] !== undefined {
  Console.log(globalThis["hello"]["bar"])
  Console.log("hello")
}

let z = Float.mod(1.2, 1.4)

let intFromBigInt = BigInt.fromString("10000000000")->BigInt.toInt

module Bugfix = {
  @obj external foo: (~bar: string=?, unit) => _ = ""
  Console.log(foo(~bar="1", ()))
}

Console.log(Int.fromString("1231231"))
Console.log(Int.fromString("12.22"))
Console.log(Int.fromString("99999999999999999"))
Console.log(Int.fromString("99999999999999999"))
Console.log(Int.fromString(~radix=2, "010101"))
