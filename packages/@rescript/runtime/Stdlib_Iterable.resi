/***
A JavaScript iterable.

Values of this type implement the iterable protocol and can be consumed by
`for...of`, `Array.from`, and other JavaScript APIs that expect iterables.

See [Iteration protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.
*/

/**
Type representing an iterable that yields values of type `'a`.
*/
type t<'a> = iterable<'a>

/**
`ignore(iterable)` marks `iterable` as intentionally unused.
*/
external ignore: t<'a> => unit = "%ignore"
