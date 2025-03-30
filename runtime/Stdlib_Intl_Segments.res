/***
 A Segments instance is an object that represents the segments of a specific string, subject to the locale and options of its constructing Intl.Segmenter instance.
https://tc39.es/ecma402/#sec-segments-objects
*/
type t

type segmentData = {
  segment: string,
  index: int,
  isWordLike: option<bool>,
  input: string,
}

@send
external containing: t => segmentData = "containing"

@send
external containingWithIndex: (t, int) => segmentData = "containing"

/**
  `ignore(segments)` ignores the provided segments and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
