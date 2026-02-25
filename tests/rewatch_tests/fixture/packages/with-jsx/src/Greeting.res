@react.component
let make = (~name) => {
  <div> {React.string("Hello, " ++ name ++ "!")} </div>
}
