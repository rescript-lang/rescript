@react.component
let make = React.forwardRef((~x: string, ref: nullable<ReactDOM.Ref.currentDomRef>) => {
  let _ = ref->Nullable.toOption->Belt.Option.map(ReactDOM.Ref.domRef)
  React.string(x)
})
