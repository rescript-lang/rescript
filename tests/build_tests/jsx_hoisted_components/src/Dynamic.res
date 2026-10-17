module DynamicSidebar = await Sidebar

let provider = DynamicSidebar.Provider.make
let element = <Sidebar.Provider>{React.string("dynamic")}</Sidebar.Provider>
