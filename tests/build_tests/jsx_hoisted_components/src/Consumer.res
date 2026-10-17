module SidebarAlias = Sidebar
module ProviderAlias = SidebarAlias.Provider

let provider = Sidebar.Provider.make
let element = <Sidebar.Provider>{React.string("child")}</Sidebar.Provider>
let deepElement = <Sidebar.Group.Provider>{React.string("deep")}</Sidebar.Group.Provider>
let aliasElement = <ProviderAlias>{React.string("alias")}</ProviderAlias>
let propsElement = <Sidebar.WithProps>{React.string("props")}</Sidebar.WithProps>
let helper = Sidebar.Provider.helper()
let memoElement = <Sidebar.Memo>{React.string("memo")}</Sidebar.Memo>
let refElement = <Sidebar.ForwardRef>{React.string("ref")}</Sidebar.ForwardRef>
let debugElement = <Sidebar.ExternalDebug>{React.string("debug")}</Sidebar.ExternalDebug>
let annotatedElement =
  <Sidebar.ExternalAnnotated>{React.string("annotated")}</Sidebar.ExternalAnnotated>
