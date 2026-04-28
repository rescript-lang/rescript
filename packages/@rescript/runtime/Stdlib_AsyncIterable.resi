/***
A JavaScript async iterable.

Values of this type implement the async iterable protocol and can be consumed
by `for await...of` and other JavaScript APIs that expect async iterables.

See [`AsyncIterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncIterator) on MDN.
*/

/**
Type representing an async iterable that yields values of type `'yield`.
*/
type t<'yield> = asyncIterable<'yield>

/**
`ignore(asyncIterable)` marks `asyncIterable` as intentionally unused.
*/
external ignore: t<'yield> => unit = "%ignore"
