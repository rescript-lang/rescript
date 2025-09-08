// <div
//      ^com

// <div testing={}
//               ^com

module SomeComponent = {
  @jsx.component
  let make = (~someProp) => {
    let someString = ""
    let someInt = 12
    let someArr = [GenericJsx.null]
    ignore(someInt)
    ignore(someArr)
    // someString->st
    //               ^com
    open GenericJsx
    <div>
      {GenericJsx.string(someProp ++ someString)}
      <div> {GenericJsx.null} </div>
      // {someString->st}
      //                ^com
    </div>
  }
}
