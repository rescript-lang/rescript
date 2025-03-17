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
