@@config({
  flags: ["-bs-jsx", "4", "-bs-jsx-preserve", "-bs-jsx-module", "Custom"],
})

module Custom = {
  type component<'props> = Jsx.component<'props>
  type element = Jsx.element

  external jsx: (component<'props>, 'props) => element = "jsx"

  type fragmentProps = {children?: element}

  external jsxFragment: component<fragmentProps> = "Fragment"
}

let _fragment = <> {Jsx.string("Hello, world!")} </>
