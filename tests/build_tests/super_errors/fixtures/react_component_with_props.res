module V4C7 = {
  @react.componentWithProps
  let make = React.forwardRef((~className=?, ~children, ref: nullable<ReactRef.currentDomRef>) =>
    <div>
      <input
        type_="text" ?className ref=?{Nullable.toOption(ref)->Belt.Option.map(React.Ref.domRef)}
      />
      children
    </div>
  )
}
