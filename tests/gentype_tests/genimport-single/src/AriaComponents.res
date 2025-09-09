@gentype.satisfies(("react-aria-components", "GroupRenderProps"))
type groupRenderProps = {
  isHovered: bool,
  isFocusWithin: bool,
  isFocusVisible: bool,
  isDisabled: bool,
  isInvalid: bool,
}

@gentype.satisfies(("react-stately", "SelectionBehavior"))
type selectionBehavior = | @as("toggle") Toggle | @as("replace") Replace

@gentype.satisfies(("react-stately", "SelectionMode"))
type selectionMode = | @as("none") None | @as("single") Single | @as("multiple") Multiple

type tableOptionsContextValue = {
  selectionMode: selectionMode,
  selectionBehavior: Null.t<selectionBehavior>,
  disallowEmptySelection: bool,
  allowsDragging: bool,
}

@gentype.satisfies(("react-aria-components", "useTableOptions")) @module("react-aria-components")
external useTableOptions: unit => tableOptionsContextValue = "useTableOptions"
