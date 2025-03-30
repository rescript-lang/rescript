let s = true
let f = Some([false])

// switch (s, f) { | }
//                  ^com

type otherRecord = {
  someField: int,
  otherField: string,
}

type rec someRecord = {
  age: int,
  offline: bool,
  online: option<bool>,
  variant: someVariant,
  polyvariant: somePolyVariant,
  nested: option<otherRecord>,
}
and someVariant = One | Two | Three(int, string)
and somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]

let fnTakingRecord = (r: someRecord) => {
  ignore(r)
}

// let _ = fnTakingRecord({})
//                         ^com

// let _ = fnTakingRecord({n})
//                          ^com

// let _ = fnTakingRecord({offline: })
//                                 ^com

// let _ = fnTakingRecord({age: 123, })
//                                  ^com

// let _ = fnTakingRecord({age: 123,  offline: true})
//                                   ^com

// let _ = fnTakingRecord({age: 123, nested: })
//                                          ^com

// let _ = fnTakingRecord({age: 123, nested: {}})
//                                            ^com

// let _ = fnTakingRecord({age: 123, nested: Some({})})
//                                                 ^com

// let _ = fnTakingRecord({age: 123, variant: })
//                                           ^com

// let _ = fnTakingRecord({age: 123, variant: O })
//                                             ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three() })
//                                                       ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three({}, ) })
//                                                          ^com

// let _ = fnTakingRecord({age: 123, polyvariant: #three({}, t) })
//                                                            ^com

let fnTakingArray = (arr: array<option<bool>>) => {
  ignore(arr)
}

// let _ = fnTakingArray()
//                       ^com

// let _ = fnTakingArray([])
//                        ^com

// let _ = fnTakingArray(s)
//                        ^com

// let _ = fnTakingArray([Some()])
//                             ^com

// let _ = fnTakingArray([None, ])
//                             ^com

// let _ = fnTakingArray([None, , None])
//                             ^com

let someBoolVar = true

// let _ = fnTakingRecord({offline: so })
//                                    ^com

let fnTakingOtherRecord = (r: otherRecord) => {
  ignore(r)
}

// let _ = fnTakingOtherRecord({otherField: })
//                                         ^com

type recordWithOptionalField = {
  someField: int,
  someOptField?: bool,
}

let fnTakingRecordWithOptionalField = (r: recordWithOptionalField) => {
  ignore(r)
}

// let _ = fnTakingRecordWithOptionalField({someOptField: })
//                                                       ^com
type recordWithOptVariant = {someVariant: option<someVariant>}

let fnTakingRecordWithOptVariant = (r: recordWithOptVariant) => {
  ignore(r)
}

// let _ = fnTakingRecordWithOptVariant({someVariant: })
//                                                   ^com

type variantWithInlineRecord =
  WithInlineRecord({someBoolField: bool, otherField: option<bool>, nestedRecord: otherRecord})

let fnTakingInlineRecord = (r: variantWithInlineRecord) => {
  ignore(r)
}

// let _ = fnTakingInlineRecord(WithInlineRecord())
//                                               ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({}))
//                                                ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({s}))
//                                                 ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({nestedRecord: }))
//                                                             ^com

// let _ = fnTakingInlineRecord(WithInlineRecord({nestedRecord: {} }))
//                                                               ^com

type variant = First | Second(bool)

let fnTakingCallback = (
  cb: unit => unit,
  cb2: bool => unit,
  cb3: ReactEvent.Mouse.t => unit,
  cb4: (~on: bool, ~off: bool=?, variant) => int,
  cb5: (bool, option<bool>, bool) => unit,
  cb6: (~on: bool=?, ~off: bool=?, unit) => int,
) => {
  let _ = cb
  let _ = cb2
  let _ = cb3
  let _ = cb4
  let _ = cb5
  let _ = cb6
}

// fnTakingCallback()
//                  ^com

// fnTakingCallback(a)
//                   ^com

// fnTakingCallback(a, )
//                    ^com

// fnTakingCallback(a, b, )
//                       ^com

// fnTakingCallback(a, b, c, )
//                           ^com

// fnTakingCallback(a, b, c, d, )
//                              ^com

// fnTakingCallback(a, b, c, d, e, )
//                                ^com

let something = {
  let second = true
  let second2 = 1
  ignore(second)
  ignore(second2)
  Console.log(s)
  //           ^com
}

let fff: recordWithOptionalField = {
  someField: 123,
  someOptField: true,
}

ignore(fff)

// fff.someOpt
//            ^com

type someTyp = {test: bool}

let takesCb = cb => {
  cb({test: true})
}

// takesCb()
//         ^com

module Environment = {
  type t = {hello: bool}
}

let takesCb2 = cb => {
  cb({Environment.hello: true})
}

// takesCb2()
//          ^com

type apiCallResult = {hi: bool}

let takesCb3 = cb => {
  cb({hi: true})
}

// takesCb3()
//          ^com

let takesCb4 = cb => {
  cb(Some({hi: true}))
}

// takesCb4()
//          ^com

let takesCb5 = cb => {
  cb([Some({hi: true})])
}

// takesCb5()
//          ^com

module RecordSourceSelectorProxy = {
  type t
}

@val
external commitLocalUpdate: (~updater: RecordSourceSelectorProxy.t => unit) => unit =
  "commitLocalUpdate"

// commitLocalUpdate(~updater=)
//                            ^com

let fnTakingAsyncCallback = (cb: unit => promise<unit>) => {
  let _ = cb
}

// fnTakingAsyncCallback()
//                       ^com

let arr = ["hello"]

// arr->Belt.Array.map()
//                     ^com

type exoticPolyvariant = [#"some exotic"]

let takesExotic = (e: exoticPolyvariant) => {
  ignore(e)
}

// takesExotic()
//             ^com

let fnTakingPolyVariant = (a: somePolyVariant) => {
  ignore(a)
}

// fnTakingPolyVariant()
//                     ^com

// fnTakingPolyVariant(#)
//                      ^com

// fnTakingPolyVariant(#o)
//                       ^com

// fnTakingPolyVariant(o)
//                      ^com

module SuperInt: {
  type t
  let increment: (t, int) => t
  let decrement: (t, int => int) => t
  let make: int => t
  let toInt: t => int
} = {
  type t = int
  let increment = (t, num) => t + num
  let decrement = (t, decrementer) => decrementer(t)
  let make = t => t
  let toInt = t => t
}

type withIntLocal = {superInt: SuperInt.t}

// let withInt: withIntLocal = {superInt: }
//                                       ^com

// CompletionSupport.makeTestHidden()
//                                  ^com

open CompletionSupport
// CompletionSupport.makeTestHidden()
//                                  ^com

let mkStuff = (r: RegExp.t) => {
  ignore(r)
  "hello"
}

// mkStuff()
//         ^com

module Money: {
  type t

  let zero: t

  let nonTType: string

  let make: unit => t

  let fromInt: int => t

  let plus: (t, t) => t
} = {
  type t = string

  let zero: t = "0"

  let nonTType = "0"

  let make = (): t => zero

  let fromInt = (int): t => int->Int.toString

  let plus = (m1, _) => m1
}

let tArgCompletionTestFn = (_tVal: Money.t) => ()

// tArgCompletionTestFn()
//                      ^com

let labeledTArgCompletionTestFn = (~tVal as _: Money.t) => ()

// labeledTArgCompletionTestFn(~tVal=)
//                                   ^com

let someTyp: someTyp = {test: true}

// switch someTyp. { | _ => () }
//                ^com

type config = {
  includeName: bool,
  operator?: [#"and" | #or],
  showMore: bool,
}

type hookReturn = {name: string}

let hook = (config: config) => {
  ignore(config)
  {
    name: "tester",
  }
}

let {name} = hook({
  //                  ^com
  // ope
  //    ^com
  includeName: true,
  showMore: true,
})

// switch someTyp. { | }
//                ^com
