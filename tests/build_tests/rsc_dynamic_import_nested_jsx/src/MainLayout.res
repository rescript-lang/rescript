module DynamicSidebar = await Sidebar

let dynamicProvider = DynamicSidebar.Provider.make

@react.component
let make = (~children) => {
  <Sidebar.Provider> {Option.getOr(children, React.null)} </Sidebar.Provider>
}
