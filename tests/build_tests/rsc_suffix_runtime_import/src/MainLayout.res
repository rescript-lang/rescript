@react.component
let make = (~children) => {
  let child = Option.getOr(children, React.null)
  <Sidebar.Provider> {child} </Sidebar.Provider>
}
