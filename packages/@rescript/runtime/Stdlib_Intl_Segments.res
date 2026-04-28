/***
Bindings to `Segments` objects produced by `Intl.Segmenter.segment`.
 A Segments instance is an object that represents the segments of a specific string, subject to the locale and options of its constructing Intl.Segmenter instance.
https://tc39.es/ecma402/#sec-segments-objects.

See [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/segment) for API details and the [ECMA-402 specification](https://tc39.es/ecma402/#sec-segments-objects) for the object shape.
*/
@notUndefined
type t

type segmentData = {
  segment: string,
  index: int,
  isWordLike: option<bool>,
  input: string,
}

/**
`containing(segments)` returns the segment data for the index supplied when iterating.

Use this when consuming `Segments` via iteration helpers where the index is already implied.
*/
@send
external containing: t => segmentData = "containing"

/**
`containingWithIndex(segments, index)` returns the segment that contains `index` within the original string.

## Examples

```rescript
let segmenter = Intl.Segmenter.make(~locales=["en"], ~options={granularity: #word})
let segments = segmenter->Intl.Segmenter.segment("Hello world")
Intl.Segments.containingWithIndex(segments, 0).segment == "Hello"
```
*/
@send
external containingWithIndex: (t, int) => segmentData = "containing"

/**
  `ignore(segments)` ignores the provided segments and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
