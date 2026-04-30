@react.component
let make = (~children) => {
  <Sidebar.Provider>
    <Sidebar.Inset> {children} </Sidebar.Inset>
  </Sidebar.Provider>
}
