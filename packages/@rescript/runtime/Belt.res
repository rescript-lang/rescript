/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*** The ReScript standard library.

Belt is currently mostly covering collection types. It has no string or date functions yet, although Belt.String is in the works. In the meantime, use [Js.String](js/string) for string functions and [Js.Date](js/date) for date functions.

## Motivation

Belt provides:

- The **highest quality** immutable data structures in JavaScript.
- Safety by default: A Belt function will never throw exceptions, unless it is
  indicated explicitly in the function name (suffix "Exn").
- Better performance and smaller code size running on the JS platform.
- Ready for [Tree Shaking](https://webpack.js.org/guides/tree-shaking/).

## Usage

To use modules from Belt, either refer to them by their fully qualified name (`Belt.List`, `Belt.Array` etc.) or open the `Belt` module by putting

## Examples

```rescript
open Belt
```

at the top of your source files. After opening Belt this way, `Array` will refer to `Belt.Array`, `List` will refer to `Belt.List` etc. in the subsequent code.

If you want to open Belt globally for all files in your project instead, you can put

```json
{
  "compiler-flags": ["-open Belt"]
}
```

into your `rescript.json`.

**Note**: this is the **only** `open` we encourage.

Example usage:

## Examples

```rescript
let someNumbers = [1, 1, 4, 2, 3, 6, 3, 4, 2]

let greaterThan2UniqueAndSorted =
  someNumbers
  ->Belt.Array.keep(x => x > 2)
  // convert to and from set to make values unique
  ->Belt.Set.Int.fromArray
  ->Belt.Set.Int.toArray // output is already sorted

Js.log2("result", greaterThan2UniqueAndSorted)
```

## Specialized Collections

For collections types like set or map, Belt provides both a generic module as well as specialized, more efficient implementations for string and int keys.

For example, Belt has the following set modules:

- [Belt.Set](belt/set)
- [Belt.Set.Int](belt/set/int)
- [Belt.Set.String](belt/set/string)

## Implementation Details

### Array access runtime safety

One common confusion comes from the way Belt handles array access. It differs from than the default standard library's.

## Examples

```rescript
let letters = ["a", "b", "c"]
let a = letters[0] // a == "a"
let capitalA = Js.String.toUpperCase(a)
let k = letters[10] // Throws an exception! The 10th index doesn't exist.
```

Because Belt avoids exceptions and returns `options` instead, this code behaves differently:

## Examples

```rescript
open Belt
let letters = ["a", "b", "c"]
let a = letters[0] // a == Some("a")
let captialA = Js.String.toUpperCase(a) // Type error! This code will not compile.
let k = letters[10] // k == None
```

Although we've fixed the problem where `k` throws an exception, we now have a type error when trying to capitalize `a`. There are a few things going on here:

- Reason transforms array index access to the function `Array.get`. So `letters[0]` is the same as `Array.get(letters, 0)`.
- The compiler uses whichever `Array` module is in scope. If you `open Belt`, then it uses `Belt.Array`.
- `Belt.Array.get` returns values wrapped in options, so `letters[0] == Some("a")`.

Fortunately, this is easy to fix:

## Examples

```rescript
open Belt
let letters = ["a", "b", "c"]
let a = letters[0]

// Use a switch statement:
let capitalA = switch a {
| Some(a) => Some(Js.String.toUpperCase(a))
| None => None
}

let k = letters[10] // k == None
```

With that little bit of tweaking, our code now compiles successfully and is 100% free of runtime errors\!

### A Special Encoding for Collection Safety

When we create a collection library for a custom data type we need a way to provide a comparator function. Take Set for example, suppose its element type is a pair of ints, it needs a custom compare function that takes two tuples and returns their order. The Set could not just be typed as Set.t (int \* int) , its customized compare function needs to manifest itself in the signature, otherwise, if the user creates another customized compare function, the two collection could mix which would result in runtime error.

We use a phantom type to solve the problem:

## Examples

```rescript
module Comparable1 = Belt.Id.MakeComparable({
  type t = (int, int)
  let cmp = ((a0, a1), (b0, b1)) =>
    switch Pervasives.compare(a0, b0) {
    | 0 => Pervasives.compare(a1, b1)
    | c => c
    }
})

let mySet1 = Belt.Set.make(~id=module(Comparable1))

module Comparable2 = Belt.Id.MakeComparable({
  type t = (int, int)
  let cmp = ((a0, a1), (b0, b1)) =>
    switch Pervasives.compare(a0, b0) {
    | 0 => Pervasives.compare(a1, b1)
    | c => c
    }
})

let mySet2 = Belt.Set.make(~id=module(Comparable2))
```

Here, the compiler would infer `mySet1` and `mySet2` having different type, so e.g. a `merge` operation that tries to merge these two sets will correctly fail.

## Examples

```rescript
let mySet1: t<(int, int), Comparable1.identity> = %todo
let mySet2: t<(int, int), Comparable2.identity> = %todo
```

`Comparable1.identity` and `Comparable2.identity` are not the same using our encoding scheme.

*/

@@warning("-49")

/** [`Belt.Id`]()

  Provide utilities to create identified comparators or hashes for
  data structures used below.

  It create a unique identifier per module of
  functions so that different data structures with slightly different
  comparison functions won't mix
*/
module Id = Belt_Id

/** [`Belt.Array`]()

  **mutable array**: Utilities functions
*/
module Array = Belt_Array

/** [`Belt.SortArray`]()

  The top level provides some generic sort related utilities.

  It also has two specialized inner modules
  [`Belt.SortArray.Int`]() and [`Belt.SortArray.String`]()
*/
module SortArray = Belt_SortArray

/** [`Belt.MutableQueue`]()

  An FIFO(first in first out) queue data structure
*/
module MutableQueue = Belt_MutableQueue

/** [`Belt.MutableStack`]()

  An FILO(first in last out) stack data structure
*/
module MutableStack = Belt_MutableStack

/** [`Belt.List`]()

  Utilities for List data type
*/
module List = Belt_List

/** [`Belt.Range`]()

  Utilities for a closed range `(from, start)`
*/
module Range = Belt_Range

/** [`Belt.Set`]()

  The top level provides generic **immutable** set operations.

  It also has three specialized inner modules
  [`Belt.Set.Int`](), [`Belt.Set.String`]() and

  [`Belt.Set.Dict`](): This module separates data from function
  which is more verbose but slightly more efficient

*/
module Set = Belt_Set

/** [`Belt.Map`](),

  The top level provides generic **immutable** map operations.

  It also has three specialized inner modules
  [`Belt.Map.Int`](), [`Belt.Map.String`]() and

  [`Belt.Map.Dict`](): This module separates data from function
  which  is more verbose but slightly more efficient
*/
module Map = Belt_Map

/** [`Belt.MutableSet`]()

  The top level provides generic **mutable** set operations.

  It also has two specialized inner modules
  [`Belt.MutableSet.Int`]() and [`Belt.MutableSet.String`]()
*/
module MutableSet = Belt_MutableSet

/** [`Belt.MutableMap`]()

  The top level provides generic **mutable** map operations.

  It also has two specialized inner modules
  [`Belt.MutableMap.Int`]() and [`Belt.MutableMap.String`]()

*/
module MutableMap = Belt_MutableMap

/** [`Belt.HashSet`]()

  The top level provides generic **mutable** hash set operations.

  It also has two specialized inner modules
  [`Belt.HashSet.Int`]() and [`Belt.HashSet.String`]()
*/
module HashSet = Belt_HashSet

/** [`Belt.HashMap`]()

  The top level provides generic **mutable** hash map operations.

  It also has two specialized inner modules
  [`Belt.HashMap.Int`]() and [`Belt.HashMap.String`]()
*/
module HashMap = Belt_HashMap

/** [`Belt.Option`]()

  Utilities for option data type.
*/
module Option = Belt_Option

/** [`Belt.Result`]()

  Utilities for result data type.
*/
module Result = Belt_Result

/** [`Belt.Int`]()

  Utilities for Int.
*/
module Int = Belt_Int

/** [`Belt.Float`]()

  Utilities for Float.
*/
module Float = Belt_Float
