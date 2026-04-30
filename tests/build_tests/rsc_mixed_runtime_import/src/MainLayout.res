@module("@rescript/runtime/lib/es6/Stdlib_Option.js")
external getOr: (option<'a>, 'a) => 'a = "getOr"

@react.component
let make = (~children) => {
  let child = getOr(children, React.null)
  <Sidebar.Provider> {child} </Sidebar.Provider>
}
