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

/***
Provides bindings to JavaScript’s `Array` functions. These bindings are optimized for pipe-first (`->`), where the array to be processed is the first parameter in the function.

Here is an example to find the sum of squares of all even numbers in an array.
Without pipe first, we must call the functions in reverse order:

## Examples

```rescript
let isEven = x => mod(x, 2) == 0
let square = x => x * x
let result = {
  open Js.Array2
  reduce(map(filter([5, 2, 3, 4, 1], isEven), square), "+", 0)
}
```

With pipe first, we call the functions in the “natural” order:

```rescript
let isEven = x => mod(x, 2) == 0
let square = x => x * x
let result = {
  open Js.Array2
  [5, 2, 3, 4, 1]->filter(isEven)->map(square)->reduce("+", 0)
}
```
*/

/**
The type used to describe a JavaScript array.
*/
type t<'a> = array<'a>

/**
A type used to describe JavaScript objects that are like an array or are iterable.
*/
type array_like<'a> = Stdlib_Array.arrayLike<'a>

/* commented out until bs has a plan for iterators
   type 'a array_iter = 'a array_like
*/

@val
/**
Creates a shallow copy of an array from an array-like object. See
[`Array.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from)
on MDN.

## Examples

```rescript
let strArr = Js.String.castToArrayLike("abcd")
Js.Array2.from(strArr) == ["a", "b", "c", "d"]
```
*/
external from: array_like<'a> => array<'a> = "Array.from"

/* ES2015 */

@val
/**
Creates a new array by applying a function (the second argument) to each item
in the `array_like` first argument.  See
[`Array.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from)
on MDN.

## Examples

```rescript
let strArr = Js.String.castToArrayLike("abcd")
let code = s => Js.String.charCodeAt(0, s)
Js.Array2.fromMap(strArr, code) == [97.0, 98.0, 99.0, 100.0]
```
*/
external fromMap: (array_like<'a>, 'a => 'b) => array<'b> = "Array.from"

/* ES2015 */

@val
/**
Returns `true` if its argument is an array; `false` otherwise. This is a runtime check, which is why the second example returns `true`---a list is internally represented as a nested JavaScript array.

## Examples

```rescript
Js.Array2.isArray([5, 2, 3, 1, 4]) == true
Js.Array2.isArray(list{5, 2, 3, 1, 4}) == true
Js.Array2.isArray("abcd") == false
```
*/
external isArray: 'a => bool = "Array.isArray"

@get
/**
Returns the number of elements in the array. See
[`Array.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length)
on MDN.
*/
external length: array<'a> => int = "length"

/* Mutator functions */

@send
/**
Copies from the first element in the given array to the designated `~to_`
position, returning the resulting array. *This function modifies the original
array.* See
[`Array.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.copyWithin(arr, ~to_=2) == [100, 101, 100, 101, 102]
arr == [100, 101, 100, 101, 102]
```
*/
external copyWithin: (t<'a>, ~to_: int) => t<'a> = "copyWithin"

/* ES2015 */

@send
/**
Copies starting at element `~from` in the given array to the designated `~to_`
position, returning the resulting array. *This function modifies the original
array.* See
[`Array.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.copyWithinFrom(arr, ~from=2, ~to_=0) == [102, 103, 104, 103, 104]
arr == [102, 103, 104, 103, 104]
```
*/
external copyWithinFrom: (t<'a>, ~to_: int, ~from: int) => t<'a> = "copyWithin"

/* ES2015 */

@send
/**
Copies starting at element `~start` in the given array up to but not including
`~end_` to the designated `~to_` position, returning the resulting array. *This
function modifies the original array.* See
[`Array.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104, 105]
Js.Array2.copyWithinFromRange(arr, ~start=2, ~end_=5, ~to_=1) == [100, 102, 103, 104, 104, 105]
arr == [100, 102, 103, 104, 104, 105]
```
*/
external copyWithinFromRange: (t<'a>, ~to_: int, ~start: int, ~end_: int) => t<'a> = "copyWithin"

/* ES2015 */

@send
/**
Sets all elements of the given array (the first arumgent) to the designated
value (the secon argument), returning the resulting array. *This function
  modifies the original array.*

See
[`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.fillInPlace(arr, 99) == [99, 99, 99, 99, 99]
arr == [99, 99, 99, 99, 99]
```
*/
external fillInPlace: (t<'a>, 'a) => t<'a> = "fill"

/* ES2015 */

@send
/**
Sets all elements of the given array (the first arumgent) from position `~from`
to the end to the designated value (the second argument), returning the
resulting array. *This function modifies the original array.* See
[`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.fillFromInPlace(arr, 99, ~from=2) == [100, 101, 99, 99, 99]
arr == [100, 101, 99, 99, 99]
```
*/
external fillFromInPlace: (t<'a>, 'a, ~from: int) => t<'a> = "fill"

/* ES2015 */

@send
/**
Sets the elements of the given array (the first arumgent) from position
`~start` up to but not including position `~end_` to the designated value (the
second argument), returning the resulting array. *This function modifies the
original array.* See
[`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.fillRangeInPlace(arr, 99, ~start=1, ~end_=4) == [100, 99, 99, 99, 104]
arr == [100, 99, 99, 99, 104]
```
*/
external fillRangeInPlace: (t<'a>, 'a, ~start: int, ~end_: int) => t<'a> = "fill"

/* ES2015 */

@send
@return(undefined_to_opt)
/**
If the array is not empty, removes the last element and returns it as
`Some(value)`; returns `None` if the array is empty. *This function modifies
the original array.* See
[`Array.pop`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.pop(arr) == Some(104)
arr == [100, 101, 102, 103]

let empty: array<int> = []
Js.Array2.pop(empty) == None
```
*/
external pop: t<'a> => option<'a> = "pop"

@send
/**
Appends the given value to the array, returning the number of elements in the
updated array. *This function modifies the original array.* See
[`Array.push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push)
on MDN.

## Examples

```rescript
let arr = ["ant", "bee", "cat"]
Js.Array2.push(arr, "dog") == 4
arr == ["ant", "bee", "cat", "dog"]
```
*/
external push: (t<'a>, 'a) => int = "push"

@send
@variadic
/**
Appends the values from one array (the second argument) to another (the first
argument), returning the number of elements in the updated array. *This
function modifies the original array.* See
[`Array.push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push)
on MDN.

## Examples

```rescript
let arr = ["ant", "bee", "cat"]
Js.Array2.pushMany(arr, ["dog", "elk"]) == 5
arr == ["ant", "bee", "cat", "dog", "elk"]
```
*/
external pushMany: (t<'a>, array<'a>) => int = "push"

@send
/**
Returns an array with the elements of the input array in reverse order. *This
function modifies the original array.* See
[`Array.reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse)
on MDN.

## Examples

```rescript
let arr = ["ant", "bee", "cat"]
Js.Array2.reverseInPlace(arr) == ["cat", "bee", "ant"]
arr == ["cat", "bee", "ant"]
```
*/
external reverseInPlace: t<'a> => t<'a> = "reverse"

@send
@return(undefined_to_opt)
/**
If the array is not empty, removes the first element and returns it as
`Some(value)`; returns `None` if the array is empty. *This function modifies
the original array.* See
[`Array.shift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/shift)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
Js.Array2.shift(arr) == Some(100)
arr == [101, 102, 103, 104]

let empty: array<int> = []
Js.Array2.shift(empty) == None
```
*/
external shift: t<'a> => option<'a> = "shift"

@send
/**
Sorts the given array in place and returns the sorted array. JavaScript sorts
the array by converting the arguments to UTF-16 strings and sorting them. See
the second example with sorting numbers, which does not do a numeric sort.
*This function modifies the original array.* See
[`Array.sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)
on MDN.

## Examples

```rescript
let words = ["bee", "dog", "ant", "cat"]
Js.Array2.sortInPlace(words) == ["ant", "bee", "cat", "dog"]
words == ["ant", "bee", "cat", "dog"]

let numbers = [3, 30, 10, 1, 20, 2]
Js.Array2.sortInPlace(numbers) == [1, 10, 2, 20, 3, 30]
numbers == [1, 10, 2, 20, 3, 30]
```
*/
external sortInPlace: t<'a> => t<'a> = "sort"

@send
/**
Sorts the given array in place and returns the sorted array. *This function
  modifies the original array.*

The first argument to `sortInPlaceWith()` is a function that compares two items
from the array and returns:

* an integer less than zero if the first item is less than the second item *
zero if the items are equal * an integer greater than zero if the first item is
greater than the second item

See
[`Array.sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)
on MDN.

## Examples

```rescript
// sort by word length
let words = ["horse", "aardvark", "dog", "camel"]
let byLength = (s1, s2) => Js.String.length(s1) - Js.String.length(s2)

Js.Array2.sortInPlaceWith(words, byLength) == ["dog", "horse", "camel", "aardvark"]

// sort in reverse numeric order
let numbers = [3, 30, 10, 1, 20, 2]
let reverseNumeric = (n1, n2) => n2 - n1
Js.Array2.sortInPlaceWith(numbers, reverseNumeric) == [30, 20, 10, 3, 2, 1]
```
*/
external sortInPlaceWith: (t<'a>, ('a, 'a) => int) => t<'a> = "sort"

@send
@variadic
/**
Starting at position `~pos`, remove `~remove` elements and then add the
elements from the `~add` array. Returns an array consisting of the removed
items. *This function modifies the original array.* See
[`Array.splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice)
on MDN.

## Examples

```rescript
let arr = ["a", "b", "c", "d", "e", "f"]
Js.Array2.spliceInPlace(arr, ~pos=2, ~remove=2, ~add=["x", "y", "z"]) == ["c", "d"]
arr == ["a", "b", "x", "y", "z", "e", "f"]

let arr2 = ["a", "b", "c", "d"]
Js.Array2.spliceInPlace(arr2, ~pos=3, ~remove=0, ~add=["x", "y"]) == []
arr2 == ["a", "b", "c", "x", "y", "d"]

let arr3 = ["a", "b", "c", "d", "e", "f"]
Js.Array2.spliceInPlace(arr3, ~pos=9, ~remove=2, ~add=["x", "y", "z"]) == []
arr3 == ["a", "b", "c", "d", "e", "f", "x", "y", "z"]
```
*/
external spliceInPlace: (t<'a>, ~pos: int, ~remove: int, ~add: array<'a>) => t<'a> = "splice"

@send
/**
Removes elements from the given array starting at position `~pos` to the end of
the array, returning the removed elements. *This function modifies the original
array.* See
[`Array.splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice)
on MDN.

## Examples

```rescript
let arr = ["a", "b", "c", "d", "e", "f"]
Js.Array2.removeFromInPlace(arr, ~pos=4) == ["e", "f"]
arr == ["a", "b", "c", "d"]
```
*/
external removeFromInPlace: (t<'a>, ~pos: int) => t<'a> = "splice"

@send
/**
Removes `~count` elements from the given array starting at position `~pos`,
returning the removed elements. *This function modifies the original array.*
See
[`Array.splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice)
on MDN.

## Examples

```rescript
let arr = ["a", "b", "c", "d", "e", "f"]
Js.Array2.removeCountInPlace(arr, ~pos=2, ~count=3) == ["c", "d", "e"]
arr == ["a", "b", "f"]
```
*/
external removeCountInPlace: (t<'a>, ~pos: int, ~count: int) => t<'a> = "splice"

@send
/**
Adds the given element to the array, returning the new number of elements in
the array. *This function modifies the original array.* See
[`Array.unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift)
on MDN.

## Examples

```rescript
let arr = ["b", "c", "d"]
Js.Array2.unshift(arr, "a") == 4
arr == ["a", "b", "c", "d"]
```
*/
external unshift: (t<'a>, 'a) => int = "unshift"

@send
@variadic
/**
Adds the elements in the second array argument at the beginning of the first
array argument, returning the new number of elements in the array. *This
function modifies the original array.* See
[`Array.unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift)
on MDN.

## Examples

```rescript
let arr = ["d", "e"]
Js.Array2.unshiftMany(arr, ["a", "b", "c"]) == 5
arr == ["a", "b", "c", "d", "e"]
```
*/
external unshiftMany: (t<'a>, array<'a>) => int = "unshift"

/* Accessor functions
 */
@send @deprecated("`append` is not type-safe. Use `concat` instead.")
external append: (t<'a>, 'a) => t<'a> = "concat"

@send
/**
Concatenates the second array argument to the first array argument, returning a
new array. The original arrays are not modified. See
[`Array.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat)
on MDN.

## Examples

```rescript
Js.Array2.concat(["a", "b"], ["c", "d", "e"]) == ["a", "b", "c", "d", "e"]
```
*/
external concat: (t<'a>, t<'a>) => t<'a> = "concat"

@send
@variadic
/**
The second argument to `concatMany()` is an array of arrays; these are added at
the end of the first argument, returning a new array. See
[`Array.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat)
on MDN.

## Examples

```rescript
Js.Array2.concatMany(["a", "b", "c"], [["d", "e"], ["f", "g", "h"]]) == [
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
  ]
```
*/
external concatMany: (t<'a>, array<t<'a>>) => t<'a> = "concat"

@send
/**
Returns true if the given value is in the array, `false` otherwise. See
[`Array.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes)
on MDN.

## Examples

```rescript
Js.Array2.includes(["a", "b", "c"], "b") == true
Js.Array2.includes(["a", "b", "c"], "x") == false
```
*/
external includes: (t<'a>, 'a) => bool = "includes"

@send
/**
Returns the index of the first element in the array that has the given value.
If the value is not in the array, returns -1. See
[`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf)
on MDN.

## Examples

```rescript
Js.Array2.indexOf([100, 101, 102, 103], 102) == 2
Js.Array2.indexOf([100, 101, 102, 103], 999) == -1
```
*/
external indexOf: (t<'a>, 'a) => int = "indexOf"

@send
/**
Returns the index of the first element in the array with the given value. The
search starts at position `~from`. See
[`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf)
on MDN.

## Examples

```rescript
Js.Array2.indexOfFrom(["a", "b", "a", "c", "a"], "a", ~from=2) == 2
Js.Array2.indexOfFrom(["a", "b", "a", "c", "a"], "a", ~from=3) == 4
Js.Array2.indexOfFrom(["a", "b", "a", "c", "a"], "b", ~from=2) == -1
```
*/
external indexOfFrom: (t<'a>, 'a, ~from: int) => int = "indexOf"

@send
/**
This function converts each element of the array to a string (via JavaScript)
and concatenates them, separated by the string given in the first argument,
into a single string. See
[`Array.join`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
on MDN.

## Examples

```rescript
Js.Array2.joinWith(["ant", "bee", "cat"], "--") == "ant--bee--cat"
Js.Array2.joinWith(["door", "bell"], "") == "doorbell"
Js.Array2.joinWith([2020, 9, 4], "/") == "2020/9/4"
Js.Array2.joinWith([2.5, 3.6, 3e-2], ";") == "2.5;3.6;0.03"
```
*/
external joinWith: (t<'a>, string) => string = "join"

@send
/**
Returns the index of the last element in the array that has the given value. If
the value is not in the array, returns -1. See
[`Array.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf)
on MDN.

## Examples

```rescript
Js.Array2.lastIndexOf(["a", "b", "a", "c"], "a") == 2
Js.Array2.lastIndexOf(["a", "b", "a", "c"], "x") == -1
```
*/
external lastIndexOf: (t<'a>, 'a) => int = "lastIndexOf"

@send
/**
Returns the index of the last element in the array that has the given value,
searching from position `~from` down to the start of the array. If the value is
not in the array, returns -1. See
[`Array.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf)
on MDN.

## Examples

```rescript
Js.Array2.lastIndexOfFrom(["a", "b", "a", "c", "a", "d"], "a", ~from=3) == 2
Js.Array2.lastIndexOfFrom(["a", "b", "a", "c", "a", "d"], "c", ~from=2) == -1
```
*/
external lastIndexOfFrom: (t<'a>, 'a, ~from: int) => int = "lastIndexOf"

@send
/**
Returns a shallow copy of the given array from the `~start` index up to but not
including the `~end_` position. Negative numbers indicate an offset from the
end of the array. See
[`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)
on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104, 105, 106]
Js.Array2.slice(arr, ~start=2, ~end_=5) == [102, 103, 104]
Js.Array2.slice(arr, ~start=-3, ~end_=-1) == [104, 105]
Js.Array2.slice(arr, ~start=9, ~end_=10) == []
```
*/
external slice: (t<'a>, ~start: int, ~end_: int) => t<'a> = "slice"

@send
/**
Returns a copy of the entire array. Same as `Js.Array2.Slice(arr, ~start=0,
~end_=Js.Array2.length(arr))`. See
[`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)
on MDN.
*/
external copy: t<'a> => t<'a> = "slice"

@send
/**
Returns a shallow copy of the given array from the given index to the end. See
[`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)
on MDN.
*/
external sliceFrom: (t<'a>, int) => t<'a> = "slice"

@send
/**
Converts the array to a string. Each element is converted to a string using
JavaScript. Unlike the JavaScript `Array.toString()`, all elements in a
ReasonML array must have the same type. See
[`Array.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString)
on MDN.

## Examples

```rescript
Js.Array2.toString([3.5, 4.6, 7.8]) == "3.5,4.6,7.8"
Js.Array2.toString(["a", "b", "c"]) == "a,b,c"
```
*/
external toString: t<'a> => string = "toString"

@send
/**
Converts the array to a string using the conventions of the current locale.
Each element is converted to a string using JavaScript. Unlike the JavaScript
`Array.toLocaleString()`, all elements in a ReasonML array must have the same
type. See
[`Array.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toLocaleString)
on MDN.

## Examples

```rescript
Js.Array2.toLocaleString([Js.Date.make()])
// returns "3/19/2020, 10:52:11 AM" for locale en_US.utf8
// returns "2020-3-19 10:52:11" for locale de_DE.utf8
```
*/
external toLocaleString: t<'a> => string = "toLocaleString"

/* Iteration functions
 */
/* commented out until bs has a plan for iterators
   external entries : 'a t -> (int * 'a) array_iter = "" [@@send] (* ES2015 *)
*/

@send
/**
The first argument to `every()` is an array. The second argument is a predicate
function that returns a boolean. The `every()` function returns `true` if the
predicate function is true for all items in the given array. If given an empty
array, returns `true`. See
[`Array.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every)
on MDN.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0
Js.Array2.every([6, 22, 8, 4], isEven) == true
Js.Array2.every([6, 22, 7, 4], isEven) == false
```
*/
external every: (t<'a>, 'a => bool) => bool = "every"

@send
/**
The first argument to `everyi()` is an array. The second argument is a
predicate function with two arguments: an array element and that element’s
index; it returns a boolean. The `everyi()` function returns `true` if the
predicate function is true for all items in the given array. If given an empty
array, returns `true`. See
[`Array.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every)
on MDN.

## Examples

```rescript
// determine if all even-index items are positive
let evenIndexPositive = (item, index) => mod(index, 2) == 0 ? item > 0 : true

Js.Array2.everyi([6, -3, 5, 8], evenIndexPositive) == true
Js.Array2.everyi([6, 3, -5, 8], evenIndexPositive) == false
```
*/
external everyi: (t<'a>, ('a, int) => bool) => bool = "every"

@send
/**
Applies the given predicate function (the second argument) to each element in
the array; the result is an array of those elements for which the predicate
function returned `true`. See
[`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
on MDN.

## Examples

```rescript
let nonEmpty = s => s != ""
Js.Array2.filter(["abc", "", "", "def", "ghi"], nonEmpty) == ["abc", "def", "ghi"]
```
*/
external filter: (t<'a>, 'a => bool) => t<'a> = "filter"

@send
/**
Each element of the given array are passed to the predicate function. The
return value is an array of all those elements for which the predicate function
returned `true`.

See
[`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
on MDN.

## Examples

```rescript
// keep only positive elements at odd indices
let positiveOddElement = (item, index) => mod(index, 2) == 1 && item > 0

Js.Array2.filteri([6, 3, 5, 8, 7, -4, 1], positiveOddElement) == [3, 8]
```
*/
external filteri: (t<'a>, ('a, int) => bool) => t<'a> = "filter"

@send
@return({undefined_to_opt: undefined_to_opt})
/**
Returns `Some(value)` for the first element in the array that satisifies the
given predicate function, or `None` if no element satisifies the predicate. See
[`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find)
on MDN.

## Examples

```rescript
// find first negative element
Js.Array2.find([33, 22, -55, 77, -44], x => x < 0) == Some(-55)
Js.Array2.find([33, 22, 55, 77, 44], x => x < 0) == None
```
*/
external find: (t<'a>, 'a => bool) => option<'a> = "find"

/* ES2015 */

@send
@return({undefined_to_opt: undefined_to_opt})
/**
Returns `Some(value)` for the first element in the array that satisifies the
given predicate function, or `None` if no element satisifies the predicate. The
predicate function takes an array element and an index as its parameters. See
[`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find)
on MDN.

## Examples

```rescript
// find first positive item at an odd index
let positiveOddElement = (item, index) => mod(index, 2) == 1 && item > 0

Js.Array2.findi([66, -33, 55, 88, 22], positiveOddElement) == Some(88)
Js.Array2.findi([66, -33, 55, -88, 22], positiveOddElement) == None
```
*/
external findi: (t<'a>, ('a, int) => bool) => option<'a> = "find"

/* ES2015 */

@send
/**
Returns the index of the first element in the array that satisifies the given
predicate function, or -1 if no element satisifies the predicate. See
[`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find)
on MDN.

## Examples

```rescript
Js.Array2.findIndex([33, 22, -55, 77, -44], x => x < 0) == 2
Js.Array2.findIndex([33, 22, 55, 77, 44], x => x < 0) == -1
```
*/
external findIndex: (t<'a>, 'a => bool) => int = "findIndex"

/* ES2015 */

@send
/**
Returns `Some(value)` for the first element in the array that satisifies the
given predicate function, or `None` if no element satisifies the predicate. The
predicate function takes an array element and an index as its parameters. See
[`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find)
on MDN.

## Examples

```rescript
// find index of first positive item at an odd index
let positiveOddElement = (item, index) => mod(index, 2) == 1 && item > 0

Js.Array2.findIndexi([66, -33, 55, 88, 22], positiveOddElement) == 3
Js.Array2.findIndexi([66, -33, 55, -88, 22], positiveOddElement) == -1
```
*/
external findIndexi: (t<'a>, ('a, int) => bool) => int = "findIndex"

/* ES2015 */

@send
/**
The `forEach()` function applies the function given as the second argument to
each element in the array. The function you provide returns `unit`, and the
`forEach()` function also returns `unit`. You use `forEach()` when you need to
process each element in the array but not return any new array or value; for
example, to print the items in an array. See
[`Array.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach)
on MDN.

## Examples

```rescript
// display all elements in an array
Js.Array2.forEach(["a", "b", "c"], x => Js.log(x)) == ()
```
*/
external forEach: (t<'a>, 'a => unit) => unit = "forEach"

@send
/**
The `forEachi()` function applies the function given as the second argument to
each element in the array. The function you provide takes an item in the array
and its index number, and returns `unit`. The `forEachi()` function also
returns `unit`. You use `forEachi()` when you need to process each element in
the array but not return any new array or value; for example, to print the
items in an array. See
[`Array.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach)
on MDN.

## Examples

```rescript
// display all elements in an array as a numbered list
Js.Array2.forEachi(["a", "b", "c"], (item, index) => Js.log2(index + 1, item)) == ()
```
*/
external forEachi: (t<'a>, ('a, int) => unit) => unit = "forEach"

/* commented out until bs has a plan for iterators
   external keys : 'a t -> int array_iter = "" [@@send] (* ES2015 *)
*/

@send
/**
Applies the function (the second argument) to each item in the array, returning
a new array. The result array does not have to have elements of the same type
as the input array. See
[`Array.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
on MDN.

## Examples

```rescript
Js.Array2.map([12, 4, 8], x => x * x) == [144, 16, 64]
Js.Array2.map(["animal", "vegetable", "mineral"], Js.String.length) == [6, 9, 7]
```
*/
external map: (t<'a>, 'a => 'b) => t<'b> = "map"

@send
/**
Applies the function (the second argument) to each item in the array, returning
a new array. The function acceps two arguments: an item from the array and its
index number. The result array does not have to have elements of the same type
as the input array. See
[`Array.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
on MDN.

## Examples

```rescript
// multiply each item in array by its position
let product = (item, index) => item * index
Js.Array2.mapi([10, 11, 12], product) == [0, 11, 24]
```
*/
external mapi: (t<'a>, ('a, int) => 'b) => t<'b> = "map"

@send
/**
The `reduce()` function takes three parameters: an array, a *reducer function*,
and a beginning accumulator value. The reducer function has two parameters: an
accumulated value and an element of the array.

`reduce()` first calls the reducer function with the beginning value and the
first element in the array. The result becomes the new accumulator value, which
is passed in to the reducer function along with the second element in the
array. `reduce()` proceeds through the array, passing in the result of each
stage as the accumulator to the reducer function.

When all array elements are processed, the final value of the accumulator
becomes the return value of `reduce()`. See
[`Array.reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce)
on MDN.

## Examples

```rescript
let sumOfSquares = (accumulator, item) => accumulator + item * item

Js.Array2.reduce([10, 2, 4], sumOfSquares, 0) == 120
Js.Array2.reduce([10, 2, 4], "*", 1) == 80
Js.Array2.reduce(
  ["animal", "vegetable", "mineral"],
  (acc, item) => acc + Js.String.length(item),
  0,
) == 22 // 6 + 9 + 7
Js.Array2.reduce([2.0, 4.0], (acc, item) => item /. acc, 1.0) == 2.0 // 4.0 / (2.0 / 1.0)
```
*/
external reduce: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduce"

@send
/**
The `reducei()` function takes three parameters: an array, a *reducer
function*, and a beginning accumulator value. The reducer function has three
parameters: an accumulated value, an element of the array, and the index of
that element.

`reducei()` first calls the reducer function with the beginning value, the
first element in the array, and zero (its index). The result becomes the new
accumulator value, which is passed to the reducer function along with the
second element in the array and one (its index). `reducei()` proceeds from left
to right through the array, passing in the result of each stage as the
accumulator to the reducer function.

When all array elements are processed, the final value of the accumulator
becomes the return value of `reducei()`. See
[`Array.reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce)
on MDN.

## Examples

```rescript
// find sum of even-index elements in array
let sumOfEvens = (accumulator, item, index) =>
  if mod(index, 2) == 0 {
    accumulator + item
  } else {
    accumulator
  }

Js.Array2.reducei([2, 5, 1, 4, 3], sumOfEvens, 0) == 6
```
*/
external reducei: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduce"

@send
/**
The `reduceRight()` function takes three parameters: an array, a *reducer
function*, and a beginning accumulator value. The reducer function has two
parameters: an accumulated value and an element of the array.

`reduceRight()` first calls the reducer function with the beginning value and
the last element in the array. The result becomes the new accumulator value,
which is passed in to the reducer function along with the next-to-last element
in the array. `reduceRight()` proceeds from right to left through the array,
passing in the result of each stage as the accumulator to the reducer function.

When all array elements are processed, the final value of the accumulator
becomes the return value of `reduceRight()`.  See
[`Array.reduceRight`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight)
on MDN.

**NOTE:** In many cases, `reduce()` and `reduceRight()` give the same result.
However, see the last example here and compare it to the example from
`reduce()`, where order makes a difference.

## Examples

```rescript
let sumOfSquares = (accumulator, item) => accumulator + item * item

Js.Array2.reduceRight([10, 2, 4], sumOfSquares, 0) == 120
Js.Array2.reduceRight([2.0, 4.0], (acc, item) => item /. acc, 1.0) == 0.5 // 2.0 / (4.0 / 1.0)
```
*/
external reduceRight: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduceRight"

@send
/**
The `reduceRighti()` function takes three parameters: an array, a *reducer
function*, and a beginning accumulator value. The reducer function has three
parameters: an accumulated value, an element of the array, and the index of
that element. `reduceRighti()` first calls the reducer function with the
beginning value, the last element in the array, and its index (length of array
minus one). The result becomes the new accumulator value, which is passed in to
the reducer function along with the second element in the array and one (its
index). `reduceRighti()` proceeds from right to left through the array, passing
in the result of each stage as the accumulator to the reducer function.

When all array elements are processed, the final value of the accumulator
becomes the return value of `reduceRighti()`. See
[`Array.reduceRight`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight)
on MDN.

**NOTE:** In many cases, `reducei()` and `reduceRighti()` give the same result.
However, there are cases where the order in which items are processed makes a
difference.

## Examples

```rescript
// find sum of even-index elements in array
let sumOfEvens = (accumulator, item, index) =>
  if mod(index, 2) == 0 {
    accumulator + item
  } else {
    accumulator
  }

Js.Array2.reduceRighti([2, 5, 1, 4, 3], sumOfEvens, 0) == 6
```
*/
external reduceRighti: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduceRight"

@send
/**
Returns `true` if the predicate function given as the second argument to
`some()` returns `true` for any element in the array; `false` otherwise.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

Js.Array2.some([3, 7, 5, 2, 9], isEven) == true
Js.Array2.some([3, 7, 5, 1, 9], isEven) == false
```
*/
external some: (t<'a>, 'a => bool) => bool = "some"

@send
/**
Returns `true` if the predicate function given as the second argument to
`somei()` returns `true` for any element in the array; `false` otherwise. The
predicate function has two arguments: an item from the array and the index
value

## Examples

```rescript
// Does any string in the array
// have the same length as its index?

let sameLength = (str, index) => Js.String.length(str) == index

// "ef" has length 2 and is it at index 2
Js.Array2.somei(["ab", "cd", "ef", "gh"], sameLength) == true
// no item has the same length as its index
Js.Array2.somei(["a", "bc", "def", "gh"], sameLength) == false
```
*/
external somei: (t<'a>, ('a, int) => bool) => bool = "some"

/* commented out until bs has a plan for iterators
   external values : 'a t -> 'a array_iter = "" [@@send] (* ES2015 *)
*/

/**
Returns the value at the given position in the array if the position is in
bounds; returns the JavaScript value `undefined` otherwise.

## Examples

```rescript
let arr = [100, 101, 102, 103]
Js.Array2.unsafe_get(arr, 3) == 103
Js.Array2.unsafe_get(arr, 4) // returns undefined
```
*/
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

/**
Sets the value at the given position in the array if the position is in bounds.
If the index is out of bounds, well, “here there be dragons.“

*This function modifies the original array.*

## Examples

```rescript
let arr = [100, 101, 102, 103]
Js.Array2.unsafe_set(arr, 3, 99)
// result is [100, 101, 102, 99];

Js.Array2.unsafe_set(arr, 4, 88)
// result is [100, 101, 102, 99, 88]

Js.Array2.unsafe_set(arr, 6, 77)
// result is [100, 101, 102, 99, 88, <1 empty item>, 77]

Js.Array2.unsafe_set(arr, -1, 66)
// you don't want to know.
```
*/
external unsafe_set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"
