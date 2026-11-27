@@config({flags: ["-bs-jsx", "4"]})

@react.component
let make = React.forwardRef((~className=?, ~children, _ref) => {
  // Constrain type variables to avoid value restriction
  let _: option<string> = className
  children
})
