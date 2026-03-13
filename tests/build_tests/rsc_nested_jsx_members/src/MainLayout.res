@react.component
let make = (~children) => {
  <Sidebar.Provider> {children} </Sidebar.Provider>
}
