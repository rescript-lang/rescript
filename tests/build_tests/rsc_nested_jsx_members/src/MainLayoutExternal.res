@react.component
let make = (~children) => {
  <SidebarExternal.Provider> {children} </SidebarExternal.Provider>
}
