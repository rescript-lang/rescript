let wrapReactForReasonReact: (
  React.component<'props>,
  'props,
  'children,
) => ReasonReact.component<
  ReasonReact.stateless,
  ReasonReact.noRetainedProps,
  ReasonReact.actionless,
>

let wrapReasonReactForReact: (
  ~component: ReasonReact.componentSpec<'a, 'b, 'c, 'd, 'e>,
  ReasonReact.jsPropsToReason<'props, 'g, 'h, 'i>,
) => React.component<'props>
