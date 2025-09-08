// test React JSX file

@react.component
let make = (~msg) => {
  <div> {msg->React.string} </div>
}
