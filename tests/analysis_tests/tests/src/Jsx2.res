module M = {
  @react.component
  let make = (~first, ~fun="", ~second="") => React.string(first ++ fun ++ second)
}

let _ = <M first="abc" />
//       ^def

// <M second=fi
//             ^com

// <M second="abc" f
//                  ^com

// let e = <M
//           ^com

@react.component
let make = (~first) => React.string(first)

let y = 44

// <M prop={A(3)} k
//                 ^com

// <M prop=A(3) k
//               ^com

// <M prop=foo(1+2) k
//                   ^com

// <M prop=list{1,2,3} k
//                      ^com

// <M prop=<N /> k
//                ^com

// <M prop=1.5 k
//              ^com

// <M prop=0X33 k
//               ^com

// <M prop=12e+3 k
//                ^com

// <M prop='z' k
//              ^com

// <M prop=`before${foo}` k
//                         ^com

// <M prop=module(@foo Three: X_int) k
//                                    ^com

// <M prop=%bs.raw("1") k
//                       ^com

let _ = <Component />
//         ^def

module Ext = {
  @react.component @module("@material-ui/core")
  external make: (~align: string=?) => React.element = "Typography"
}

let _ = Ext.make

// <Ext al
//        ^com

// <M first
//         ^com

// <M first=#a k
//              ^com

// <M first =  ?   #a k
//                     ^com

// <M>
//    ^com

module WithChildren = {
  @react.component
  let make = (~name as _: string, ~children) => <jsx> children </jsx>
}

let _ = <WithChildren name=""> <div /> </WithChildren>
// <WithChildren
//              ^com
// <WithChildren n
//                ^com

// let c : React.e
//                ^com
// let c : ReactDOMR
//                  ^com

module DefineSomeFields = {
  type r = {thisField: int, thatField: string}
  let thisValue = 10
  // let foo x = x.th
  //                 ^com
}

// let q = DefineSomeFields.
//                          ^com
// let foo x = x.DefineSomeFields.th
//                                  ^com

let _ = x => x.DefineSomeFields.thisField + DefineSomeFields.thisValue

module Outer = {
  module Inner = {
    let hello = 3
  }
}
let _ = Outer.Inner.hello

let _ =
  <div
  // x=Outer.Inner.h
  //                ^com
    name=""
  />

let _ =
  <div
  // x=Outer.Inner.
  //               ^com
    name=""
  />

let _ =
  <div
  // x=
  //   ^com
    name=""
  />

module Nested = {
  module Comp = {
    @react.component
    let make = (~name) => React.string(name)
  }
}

let _ = <Nested.Comp name="" />

// let _ = <Nested.Co name="" />
//                   ^com

// let _ = <Nested. name="" />
//                 ^com

module Comp = {
  @react.component
  let make = (~age) => React.int(age)
}

let _ = {
  <> <Comp age=34 /> </>
  //        ^hov
}

let _ = {
  <> {<> <Comp age=34 /> </>} </>
  //            ^hov
}

module type ExtT = module type of Ext

let _ = module(Ext: ExtT)
