/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
Provides functions for inspecting and manipulating native JavaScript objects
*/

/** `empty()` returns the empty object `{}` */
@deprecated({
  reason: "Use `Object.make` instead.",
  migrate: Object.make(),
})
@obj
external empty: unit => {..} = ""

/**
`assign(target, source)` copies properties from source to target.
Properties in `target` will be overwritten by properties in `source` if they have the same key.
Returns `target`.

**See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign)

## Examples

```rescript
/* Copy an object */

let obj = {"a": 1}

let copy = Js.Obj.assign(Js.Obj.empty(), obj)

/* prints "{ a: 1 }" */
Js.log(copy)

/* Merge objects with same properties */

let target = {"a": 1, "b": 1}
let source = {"b": 2}

let obj = Js.Obj.assign(target, source)

/* prints "{ a: 1, b: 2 }" */
Js.log(obj)

/* prints "{ a: 1, b: 2 }", target is modified */
Js.log(target)
```
*/
@deprecated({
  reason: "Use `Object.assign` instead.",
  migrate: Object.assign(),
})
@val
external assign: ({..}, {..}) => {..} = "Object.assign"

/* TODO:

   Should we map this API as directly as possible, provide some abstractions, or deliberately nerf it?

   "static":
   - Object.create
   - Object.defineProperty
   - Object.defineProperties
   - Object.entries - experimental
   - Object.getOwnPropertyDescriptor
   - Object.getOwnPropertyDescriptors
   - Object.getOwnPropertyNames
   - Object.getOwnPropertySymbols
   - Object.getPrototypeOf
   - Object.isExtensible
   - Object.isFrozen
   - Object.isSealed
   - Object.preventExtension
   - Object.seal
   - Object.setPrototypeOf
   - Object.values - experimental

   send:
   - hasOwnProperty
   - isPrototypeOf
   - propertyIsEnumerable
   - toLocaleString
   - toString

   Put directly on Js?
   - Object.is
*/

/** `keys(obj)` returns an `array` of the keys of `obj`'s own enumerable properties. */
@deprecated({
  reason: "Use `Object.keysToArray` instead.",
  migrate: Object.keysToArray(),
})
@val
external keys: {..} => array<string> = "Object.keys"
