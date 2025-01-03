let getBool = () => true
let getInt = () => 123

type someRecord = {name: string, age: int}

let someFnWithCallback = (cb: (~num: int, ~someRecord: someRecord, ~isOn: bool) => unit) => {
  let _ = cb
}

let reactEventFn = (cb: ReactEvent.Mouse.t => unit) => {
  let _ = cb
}

@val external getSomeRecord: unit => someRecord = "getSomeRecord"

// let x = 123; let aliased = x; aliased->t
//                                         ^com

// let x = getSomeRecord(); x.
//                            ^com

// let x = getSomeRecord(); let aliased = x; aliased.
//                                                   ^com

// someFnWithCallback((~someRecord, ~num, ~isOn) => someRecord.)
//                                                             ^com

// let aliasedFn = someFnWithCallback; aliasedFn((~num, ~someRecord, ~isOn) => someRecord.)
//                                                                                        ^com

// reactEventFn(event => { event->pr });
//                                  ^com

module Div = {
  @react.component
  let make = (~onMouseEnter: option<JsxEvent.Mouse.t => unit>=?) => {
    let _ = onMouseEnter
    React.null
  }
}

// let _ = <div onMouseEnter={event => { event->pr }} />
//                                                ^com

// let _ = <Div onMouseEnter={event => { event->pr }} />
//                                                ^com

// let _ = <div onMouseEnter={event => { let btn = event->JsxEvent.Mouse.button; btn->t }} />
//                                                                                     ^com

// let _ = <div onMouseEnter={event => { let btn = event->JsxEvent.Mouse.button->Belt.Int.toString; btn->spl }} />
//                                                                                                          ^com

// let _ = <div onMouseEnter={event => { let btn = event->JsxEvent.Mouse.button->Belt.Int.toString->Js.String2.split("/"); btn->ma }} />
//                                                                                                                                ^com

type someVariant = One | Two | Three(int, string)
type somePolyVariant = [#one | #two | #three(int, string)]
type someNestedRecord = {someRecord: someRecord}

type someRecordWithNestedStuff = {
  things: string,
  someInt: int,
  srecord: someRecord,
  nested: someNestedRecord,
  someStuff: bool,
}

type otherNestedRecord = {
  someRecord: someRecord,
  someTuple: (someVariant, int, somePolyVariant),
  optRecord: option<someRecord>,
}

// Destructure record
// let x: someRecordWithNestedStuff = Obj.magic(); let {srecord} = x; srecord.
//                                                                            ^com

// Follow aliased
// let x: someRecordWithNestedStuff = Obj.magic(); let {nested: aliased} = x; aliased.
//                                                                                    ^com

// Follow nested record
// let x: someRecordWithNestedStuff = Obj.magic(); let {srecord, nested: {someRecord}} = x; someRecord.
//                                                                                                     ^com

// Destructure string
// let x: someRecordWithNestedStuff = Obj.magic(); let {things} = x; things->slic
//                                                                               ^com

// Destructure int
// let x: someRecordWithNestedStuff = Obj.magic(); let {someInt} = x; someInt->toS
//                                                                                ^com

// Follow tuples
// let x: otherNestedRecord = Obj.magic(); let {someTuple} = x; let (_, someInt, _) = someTuple; someInt->toS
//                                                                                                           ^com

// Same as above, but follow in switch case
// let x: otherNestedRecord; switch x { | {someTuple} => let (_, someInt, _) = someTuple; someInt->toS }
//                                                                                                    ^com

// Follow variant payloads
// let x: otherNestedRecord; switch x { | {someTuple:(Three(_, str), _, _)} => str->slic }
//                                                                                      ^com

// Follow polyvariant payloads
// let x: otherNestedRecord; switch x { | {someTuple:(_, _, #three(_, str))} => str->slic }
//                                                                                       ^com

// Follow options
// let x: otherNestedRecord; switch x { | {optRecord:Some({name})} => name->slic }
//                                                                              ^com

// Infer top level return
// let x = 123; switch x { | 123 => () | v => v->toSt }
//                                                   ^com

let fnWithRecordCallback = (cb: someRecord => unit) => {
  let _ = cb
}

// Complete pattern of function parameter
// fnWithRecordCallback(({}) => {()})
//                        ^com

let fn2 = (~cb: CompletionSupport.Nested.config => unit) => {
  let _ = cb
}

// fn2(~cb=({root}) => {root-> })
//                            ^com

type sameFileRecord = {root: CompletionSupport.Test.t, test: int}

let fn3 = (~cb: sameFileRecord => unit) => {
  let _ = cb
}

// fn3(~cb=({root}) => {root-> })
//                            ^com

// Handles pipe chains as input for switch
// let x = 123; switch x->Belt.Int.toString { | }
//                                             ^com

// Handles pipe chains as input for switch
// let x = 123; switch x->Belt.Int.toString->Js.String2.split("/") { | }
//                                                                    ^com

// Regular completion works
// let renderer = CompletionSupport2.makeRenderer(~prepare=() => "hello",~render=({support}) => {support.},())
//                                                                                                       ^com

// But pipe completion gets the wrong completion path. Should be `ReactDOM.Client.Root.t`, but ends up being `CompletionSupport2.ReactDOM.Client.Root.t`.
// let renderer = CompletionSupport2.makeRenderer(~prepare=() => "hello",~render=({support:{root}}) => {root->},())
//                                                                                                            ^com

// Handles reusing the same name already in scope for bindings
let res = 1
// switch res { | res => res }
//                         ^hov
