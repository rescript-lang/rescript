module SidebarAlias = Sidebar
module ProviderAlias = SidebarAlias.Provider

@react.component
let make = (~children) => {
  <ProviderAlias> {children} </ProviderAlias>
}
