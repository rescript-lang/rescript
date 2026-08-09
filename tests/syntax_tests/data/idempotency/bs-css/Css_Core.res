module type CssImplementationIntf = {
  let mergeStyles: (array<string>) => string
  let injectRule: (JSON.t) => unit
  let injectRaw: (string) => unit
  let make: (JSON.t) => string
  let makeKeyFrames: (dict<JSON.t>) => string
}
