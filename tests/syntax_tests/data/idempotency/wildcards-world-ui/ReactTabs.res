@module("react-tabs") @react.component
external make: (
  ~selectedIndex: int=?,
  ~onSelect: (int, int) => bool=?,
  ~children: React.element,
) => React.element = "Tabs"

module Tab = {
  @module("react-tabs") @react.component
  external make: (~children: React.element) => React.element = "Tab"
}
module TabList = {
  @module("react-tabs") @react.component
  external make: (~children: React.element) => React.element = "TabList"
}
module TabPanel = {
  @module("react-tabs") @react.component
  external make: (~children: React.element) => React.element = "TabPanel"
}
