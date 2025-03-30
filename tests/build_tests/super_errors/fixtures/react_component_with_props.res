module V4C7 = {
  @react.componentWithProps
  let make = React.forwardRef((
    ~className=?,
    ~children,
    ref: Js.Nullable.t<ReactRef.currentDomRef>,
  ) =>
    <div>
      <input
        type_="text" ?className ref=?{Js.Nullable.toOption(ref)->Belt.Option.map(React.Ref.domRef)}
      />
      children
    </div>
  )
}
