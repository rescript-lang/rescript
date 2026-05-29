module Provider = {
  @react.component @module("./SidebarExternalImpl.js")
  external make: (~children: React.element=?) => React.element = "default"
}
