module Array = Ocaml_Array

@unboxed
type t = A | I(int) | S(string)
@unboxed
type t2 = S2(string) | I2(float)

let i = I(42)
let i2 = I2(42.5)
let s = S("abc")
let s2 = S2("abc")

let classify = x =>
  switch x {
  | I(_) => "An integer"
  | S(s) => "A string" ++ s
  | A => "A"
  }

let classify2 = x =>
  switch x {
  | I2(_) => "A float"
  | S2(s) => "A string" ++ s
  }

@unboxed
type tt = One | Two | Object({x: int, y: string})

let w = Object({x: 10, y: ""})

let cls = x =>
  switch x {
  | One => "one"
  | Two => "two"
  | Object({y}) => "object" ++ y
  }

module ListWithTuples = {
  @unboxed
  type rec t<'a> = | @as(undefined) Empty | Cons(('a, t<'a>))
}

module ListWithObjects = {
  @unboxed
  type rec t<'a> = | @as(null) Empty | Cons({hd: 'a, tl: t<'a>})
}

let rec tuplesToObjects = (l: ListWithTuples.t<_>): ListWithObjects.t<_> =>
  switch l {
  | Empty => Empty
  | Cons((hd, tl)) => Cons({hd, tl: tuplesToObjects(tl)})
  }

let l1 = ListWithTuples.Cons((1, Cons((2, Cons((3, Empty))))))
let l2 = tuplesToObjects(l1)
Js.log2("l1", l1)
Js.log2("l2", l2)

module Truthy = {
  @unboxed
  type t = | @as(true) True | Obj({flag: bool})

  let isTrue = x =>
    switch x {
    | True => true
    | Obj({flag}) => flag
    }
}

module TwoObjects = {
  @unboxed
  type t = | @as(null) Null | Object({name: string}) | @as(undefined) Undefined

  let classify = x =>
    switch x {
    | Null => "null"
    | Object({name}) => "object" ++ name
    | Undefined => "undefined"
    }
}

module Unknown = {
  @unboxed
  type t<'a> = A | B | Unknown('a)

  let classify = x =>
    switch x {
    | A => "a"
    | B => "b"
    | Unknown(v) => {
        Js.log(x)
        "Unknown"
      }
    }
}

module MultipleBlocks = {
  @unboxed
  type t<'a> = A | B | C | D | String(string) | Int(int) | Object({name: string})

  let classify = x =>
    switch x {
    | A => "a"
    | B => "b"
    | C => "c"
    | D => "d"
    | String(_) => "string"
    | Int(_) => "int"
    | Object({name}) => "Object" ++ name
    }
}

module OnlyBlocks = {
  @unboxed
  type t<'a> = String(string) | Int(int) | Object({name: string})

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Int(_) => "int"
    | Object({name}) => "Object" ++ name
    }
}

module WithArray = {
  @unboxed
  type t<'a> = String(string) | Float(float) | Array(array<string>) | Object({name: string})

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Float(_) => "int"
    | Array(_) if true => "array"
    | Array(_) => "array"
    | Object({name}) => "Object" ++ name
    }
}

module Json = {
  @unboxed
  type rec t =
    | @as(false) False
    | @as(true) True
    | @as(null) Null
    | String(string)
    | Number(float)
    | Object(dict<t>)
    | Array(array<t>)

  type tagged_t =
    | JSONFalse
    | JSONTrue
    | JSONNull
    | JSONString(string)
    | JSONNumber(float)
    | JSONObject(dict<t>)
    | JSONArray(array<t>)

  let classify = (x: t) =>
    switch x {
    | False => JSONFalse
    | True => JSONTrue
    | Null => JSONNull
    | String(s) => JSONString(s)
    | Number(n) => JSONNumber(n)
    | Object(o) => JSONObject(o)
    | Array(a) => JSONArray(a)
    }

  /* from js_json.ml
let classify  (x : t) : tagged_t =
  let ty = Js.typeof x in
  if ty = "string" then
    JSONString (Obj.magic x)
  else if ty = "number" then
    JSONNumber (Obj.magic x )
  else if ty = "boolean" then
    if (Obj.magic x) = true then JSONTrue
    else JSONFalse
  else if (Obj.magic x) == Js.null then
    JSONNull
  else if Js_array2.isArray x  then
    JSONArray (Obj.magic x)
  else
    JSONObject (Obj.magic x)  
 */
}

module TrickyNested = {
  @unboxed
  type rec t =
    | A((t, t))
    | B

  let check = (s, y) =>
    switch s {
    | A((A(x), B)) if x !== y => 41
    | _ => 42
    }
}

module OverlapString = {
  @unboxed
  type enum = One | Two | Three | FutureAddedValue(string)

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | FutureAddedValue(s) => "Something else..." ++ s
    }
}

module OverlapNumber = {
  @unboxed
  type enum = | @as(1.0) One | Two | Three | FutureAddedValue(float)

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | FutureAddedValue(_) => "Something else..."
    }
}

module OverlapObject = {
  @unboxed
  type enum = | @as(null) One | Two | Three | Object({x: int})

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | Object(_) => "Object..."
    }
}

module RecordIsObject = {
  // @unboxed
  // this is not allowed
  type r = {x: int}

  @unboxed
  type t = Array(array<int>) | Record(r)

  let classify = v =>
    switch v {
    | Record({x}) => x
    | Array(a) => a[0]
    }
}

module ArrayAndObject = {
  @unboxed
  type t = Record({x: int}) | Array(array<int>)

  let classify = v =>
    switch v {
    | Record({x}) => x
    | Array(a) => a[0]
    }
}

module OptionUnboxingHeuristic = {
  type hasNull = | @as(null) Null | B(int)
  let testHasNull = (x: hasNull) => Some(x)

  type hasUndefined = | @as(undefined) Undefined | B(int)
  let testHasUndefined = (x: hasUndefined) => Some(x)

  @unboxed
  type untaggedWithOptionPayload = A | B(option<string>)
  let untaggedWithOptionPayload = (x: untaggedWithOptionPayload) => Some(x)

  @unboxed
  type untaggedWithIntPayload = A | B(int)
  let untaggedWithIntPayload = (x: untaggedWithIntPayload) => Some(x)

  @unboxed
  type untaggedInlineNoOption = A | B({x: int})
  let untaggedInlineNoOptions = (x: untaggedInlineNoOption) => Some(x)

  @unboxed
  type untaggedInlineUnaryWihtExplicitOption = A | B({x: option<int>})
  let untaggedInlineUnaryWihtExplicitOption = (x: untaggedInlineUnaryWihtExplicitOption) => Some(x)

  @unboxed
  type untaggedInlineUnaryWihtImplicitOption = A | B({x?: int})
  let untaggedInlineUnaryWihtImplicitOption = (x: untaggedInlineUnaryWihtImplicitOption) => Some(x)

  @unboxed
  type untaggedInlineMultinaryOption = A | B({x: option<int>, y?: string})
  let untaggedInlineMultinaryOption = (x: untaggedInlineMultinaryOption) => Some(x)
}

module TestFunctionCase = {
  @unboxed
  type t = Array(array<int>) | Record({x: int}) | Function(int => int)

  let classify = v =>
    switch v {
    | Record({x}) => x
    | Array(a) => a[0]
    | Function(f) => f(3)
    }

  let ff = Function(x => x + 1)
}

module ComplexPattern = {
  @unboxed
  type rec t =
    | @as(undefined) Missing
    | @as(false) False
    | @as(true) True
    | @as(null) Null
    | String(string)
    | Number(float)
    | Object(dict<t>)
    | Array(array<t>)

  type tagged_t =
    | JSONFalse
    | JSONTrue
    | JSONNull
    | JSONString(string)
    | JSONNumber(float)
    | JSONObject(dict<t>)
    | JSONArray(array<t>)

  let someJson: t = %raw(`'[{"name": "Haan"}, {"name": "Mr"}, false]'`)->Obj.magic

  let check = s =>
    switch s {
    | Array([True, False, Array([String("My name is"), Number(10.)])]) => Js.log("yup")
    | _ => Js.log("Nope...")
    }
}

module PromiseSync = {
  type user = {name: string}

  @unboxed type value = Sync(user) | Async(promise<user>) | Name(string)

  let getUserName = async (u: value) =>
    switch u {
    | Sync(user) => user.name
    | Async(userPromise) =>
      let user = await userPromise
      user.name
    | Name(name) => name
    }

  let awaitUser = async (u: value) =>
    switch u {
    | Async(userPromise) =>
      let user = await userPromise
      user.name
    | _ => "dummy"
    }
}

module Arr = {
  type record = {userName: string}

  @unboxed
  type arr =
    | Array(array<string>)
    | String(string)
    | Promise(promise<string>)
    | Object(record)
    | @as("test") Test
    | @as(12) TestInt

  let classify = async (a: arr) =>
    switch a {
    | Array(arr) => Js.log(arr->Belt.Array.joinWith("-", x => x))
    | String(s) => Js.log(s)
    | Promise(p) => Js.log(await p)
    | Object({userName}) => Js.log(userName)
    | Test => Js.log("testing")
    | TestInt => Js.log(12)
    }
}

module AllInstanceofTypes = {
  type record = {userName: string}

  @get external fileName: Js.File.t => string = "name"
  @get external blobSize: Js.Blob.t => float = "size"

  @unboxed
  type t =
    | String(string)
    | Array(array<string>)
    | Promise(promise<string>)
    | Object(record)
    | Date(Js.Date.t)
    | RegExp(Stdlib_RegExp.t)
    | File(Js.File.t)
    | Blob(Js.Blob.t)

  let classifyAll = async (t: t) =>
    switch t {
    | String(s) => Js.log(s)
    | Promise(p) => Js.log(await p)
    | Object({userName}) => Js.log(userName)
    | Date(date) => Js.log(date->Js.Date.toString)
    | RegExp(re) => Js.log(re->Js.Re.test_("test"))
    | Array(arr) => Js.log(arr->Belt.Array.joinWith("-", x => x))
    | File(file) => Js.log(file->fileName)
    | Blob(blob) => Js.log(blob->blobSize)
    }
}

module Aliased = {
  type dict = dict<string>
  type fn = unit => option<string>
  @unboxed type t = Object(dict) | String(string) | Function(fn)

  let test = (t: t) => {
    switch t {
    | Object(d) => d->Js.Dict.get("Hello")
    | String(s) => Some(s)
    | Function(fn) => fn()
    }
  }
}

module OnlyOne = {
  @unboxed type onlyOne = OnlyOne
  let onlyOne = OnlyOne
}

module MergeCases = {
  type obj = {name: string}

  @unboxed
  type t =
    | Boolean(bool)
    | Object(obj)
    | Array(array<int>)
    | Date(Js.Date.t)

  let should_not_merge = x =>
    switch x {
    | Object(_) => "do not merge"
    | Array(_) => "do not merge"
    | Date(_) => "do not merge"
    | Boolean(_) => "boolean"
    }

  let can_merge = x =>
    switch x {
    | Object(_) => "merge"
    | Array(_) => "do not merge"
    | Date(_) => "do not merge"
    | Boolean(_) => "merge"
    }
}

module ObjectAndNull = {
  let printLength = (json: JSON.t) =>
    switch json {
    | Object(o) => Console.log2("Length: ", o->Dict.valuesToArray->Array.length)
    | _ => ()
    }
}

module RecursiveType = {
  type rec object2 = {foo: string}
  @unboxed and tagged2 = Object(object2) | Fn(unit => object2)

  let o = Object({foo: "hello"})
}
