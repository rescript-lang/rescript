external componentToReasonReactClass: React.component<'props> => ReasonReact.reactClass =
  "%identity"

external reasonReactClassToComponent: ReasonReact.reactClass => React.component<'props> =
  "%identity"
let wrapReactForReasonReact = (component, props, children) =>
  ReasonReact.wrapJsForReason(~reactClass=componentToReasonReactClass(component), ~props, children)

let wrapReasonReactForReact = (~component, propsConverter) =>
  reasonReactClassToComponent(ReasonReact.wrapReasonForJs(~component, propsConverter))
