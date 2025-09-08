@react.component
let make = React.forwardRef((~x: string, ref: Js.Nullable.t<ReactDOM.Ref.currentDomRef>) => {
  let _ = ref->Js.Nullable.toOption->Belt.Option.map(ReactDOM.Ref.domRef)
  React.string(x)
})
