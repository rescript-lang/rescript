// A variable as the source of a record spread whose fields are all overridden
// leaves no trace in the output: `override` and `overrideJsx` are still small
// enough to be inlined, and `overridden` is emitted before `base`, as a record
// without the spread would be.
@@warning("-23")
@@config({flags: ["-bs-jsx", "4"]})

type point = {x: int, y: int}

let base = {x: 1, y: 2}
let overridden = {...base, x: 3, y: 4}

let _ = overridden

let override = (source, x) => {...source, x, y: 2}

let callOverride = x => override({x: 0, y: 0}, x).x

module Component = {
  type props = {a: int, b: int}

  @react.componentWithProps
  let make = props => React.int(props.a + props.b)
}

let overrideJsx = props => <Component {...props} a=1 b=2 />

let callOverrideJsx = props => overrideJsx(props)
