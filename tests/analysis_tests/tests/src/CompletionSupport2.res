module Internal = {
  type prepareProps<'prepared> = {
    someName: string,
    support: CompletionSupport.Nested.config,
    prepared: 'prepared,
  }
}

let makeRenderer = (
  ~prepare: unit => 'prepared,
  ~render: Internal.prepareProps<'prepared> => React.element,
  (),
) => {
  let _ = prepare
  let _ = render
  "123"
}
