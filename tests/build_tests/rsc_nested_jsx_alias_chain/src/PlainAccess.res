module SidebarAlias = Sidebar
module ProviderAlias = SidebarAlias.Provider

let provider = ProviderAlias.make

let callProvider = ProviderAlias.make({children: React.null})
