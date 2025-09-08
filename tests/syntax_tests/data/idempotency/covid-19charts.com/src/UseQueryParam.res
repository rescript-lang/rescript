let hook = (makeInitial, ~queryFragment, ~coder) => {
  let isInitialRender = React.useRef(true)
  let (value, setValue) = React.useState(makeInitial)
  React.useEffect1(() =>
    if React.Ref.current(isInitialRender) {
      let pathname = SerializeQueryParam.parse(Window.window.location.search)

      {
        open Belt.Option
        forEach(
          flatMap(Js.Dict.get(pathname, queryFragment), coder.SerializeQueryParam.decode),
          x => setValue(_ => x),
        )
      }

      React.Ref.setCurrent(isInitialRender, false)
      None
    } else {
      let obj = Js.Dict.empty()
      Js.Dict.set(obj, queryFragment, coder.encode(value))
      let {
        SerializeQueryParam.protocol: protocol,
        host,
        pathname,
        search,
        state,
      } = SerializeQueryParam.updateInLocation(obj, Window.window.location)

      {
        open Window
        window.history.replaceState(. state, "", `$protocol//$host$pathname$search`)
      }

      None
    }
  , [value])
  (value, setValue)
}
