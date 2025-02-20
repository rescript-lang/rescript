let someString = "hello"
ignore(someString)

// someString->st
//               ^com

module SomeComponent = {
  @react.component
  let make = (~someProp) => {
    let someInt = 12
    let someArr = [React.null]
    ignore(someInt)
    ignore(someArr)
    // someString->st
    //               ^com
    <div>
      {React.string(someProp)}
      <div> {React.null} </div>
      // {someString->st}
      //                ^com
      // {"Some string"->st}
      //                   ^com
      // {"Some string"->String.trim->st}
      //                                ^com
      // {someInt->}
      //           ^com
      // {12->}
      //      ^com
      // {someArr->a}
      //            ^com
      // <di
      //    ^com
    </div>
  }
}

module CompWithoutJsxPpx = {
  type props = {name: string}

  let make = ({name}) => {
    ignore(name)
    React.null
  }
}

// <CompWithoutJsxPpx n
//                     ^com

// <SomeComponent someProp=>
//                         ^com

// <h1 hidd
//         ^com

module IntrinsicElementLowercase = {
  type props = {name?: string, age?: int}

  @module("react")
  external make: (@as("mesh") _, props) => Jsx.element = "createElement"
}

// <IntrinsicElementLowercase
//                            ^com

module MultiPropComp = {
  type time = Now | Later
  @react.component
  let make = (~name, ~age, ~time: time) => {
    ignore(time)
    name ++ age
  }
}

// <MultiPropComp name="Hello" time= age="35"
//                                  ^com

// <MultiPropComp name="Hello" time= age
//                                  ^com

// <MultiPropComp name time= age
//                          ^com

module Info = {
  @react.component
  let make = (~_type: [#warning | #info]) => {
    React.string((_type :> string))
  }
}

// <Info _type={#warning} >
//                        ^com


// let _ = <p>{"".s}</p>
//                 ^com
