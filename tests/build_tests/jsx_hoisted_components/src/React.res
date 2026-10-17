type element = Jsx.element
type component<'props> = Jsx.component<'props>
type componentLike<'props, 'return> = 'props => 'return

external component: componentLike<'props, element> => component<'props> = "%component_identity"
external string: string => element = "%identity"

@module("./jsx-runtime.js")
external jsx: (component<'props>, 'props) => element = "jsx"

@module("./jsx-runtime.js")
external memo: component<'props> => component<'props> = "memo"

@module("./jsx-runtime.js")
external forwardRef: (('props, nullable<'ref>) => element) => component<'props> = "forwardRef"
