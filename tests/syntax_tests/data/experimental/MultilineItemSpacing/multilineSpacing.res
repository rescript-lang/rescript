module Details = {
  @react.component
  let make = (~input as {filePath}: input) => React.string(filePath)
}
module Body = {
  @react.component
  let make = (~input as {filePath, oldString}: input) => <>
    <ToolView.Field label="file" value=filePath />
    <ToolView.Text label="replace" text=oldString />
  </>
}
module Output = Tool.TextResult.Output
let failed = Tool.TextResult.failed

// one-line items stay together, a comment above one does not count
let a = 1
// about b
let b = 2
/** a doc comment prints on its own line */
let c = 3
let d = 4 // trailing

// an item that only breaks because of the line width is multi-line
let long = someFunctionWithAVeryLongName(argumentNumberOne, argumentNumberTwo, argumentNumber3, argument4)
let e = 5
type r = {
  x: int,
}
let f = 6

// an own-line trailing comment of the previous item goes below the blank line
f(a
  // relocated
  => 1)
let g = {
  1
}

// blocks are not affected
let h = () => {
  let x = 1
  let y = {
    2
  }
  x + y
}
