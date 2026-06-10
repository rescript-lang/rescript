type t<'param, 'output> = taggedTemplate<'param, 'output>

// A tagged template tag is invoked by the JS engine as `tag(strings, ...values)`.
// `make` adapts a plain ReScript tag function — which receives the interpolated
// values bundled into a single array — to that variadic call convention.
let make = %raw(`transform => (strings, ...values) => transform(strings, values)`)
