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

/*** JavaScript String API */

@@warning("-103")

@deprecated({
  reason: "Use `string` directly instead.",
  migrate: %replace.type(: string),
})
type t = string

/**
`make(value)` converts the given value to a `string`.

## Examples

```rescript
Js.String2.make(3.5) == "3.5"
Js.String2.make([1, 2, 3]) == "1,2,3"
```
*/
@deprecated({
  reason: "Use `String.make` instead.",
  migrate: String.make(),
})
@val
external make: 'a => t = "String"

/**
`fromCharCode(n)` creates a `string` containing the character corresponding to that number; `n` ranges from 0 to 65535.
If out of range, the lower 16 bits of the value are used. Thus, `fromCharCode(0x1F63A)` gives the same result as `fromCharCode(0xF63A)`. See [`String.fromCharCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode) on MDN.

## Examples

```rescript
Js.String2.fromCharCode(65) == "A"
Js.String2.fromCharCode(0x3c8) == `ψ`
Js.String2.fromCharCode(0xd55c) == `한`
Js.String2.fromCharCode(-64568) == `ψ`
```
*/
@deprecated({
  reason: "Use `String.fromCharCode` instead.",
  migrate: String.fromCharCode(),
})
@val
external fromCharCode: int => t = "String.fromCharCode"

/**
`fromCharCodeMany([n1, n2, n3])` creates a `string` from the characters
corresponding to the given numbers, using the same rules as `fromCharCode`. See
[`String.fromCharCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode)
on MDN.
*/
@deprecated({
  reason: "Use `String.fromCharCodeMany` instead.",
  migrate: String.fromCharCodeMany(),
})
@val
@variadic
external fromCharCodeMany: array<int> => t = "String.fromCharCode"

/**
`fromCodePoint(n)` creates a `string` containing the character corresponding to
that numeric code point. If the number is not a valid code point, it throws
`RangeError`.Thus, `fromCodePoint(0x1F63A)` will produce a correct value,
unlike `fromCharCode(0x1F63A)`, and `fromCodePoint(-5)` will throw a
`RangeError`.

See [`String.fromCodePoint`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCodePoint)
on MDN.

## Examples

```rescript
Js.String2.fromCodePoint(65) == "A"
Js.String2.fromCodePoint(0x3c8) == `ψ`
Js.String2.fromCodePoint(0xd55c) == `한`
Js.String2.fromCodePoint(0x1f63a) == `😺`
```
*/
@deprecated({
  reason: "Use `String.fromCodePoint` instead.",
  migrate: String.fromCodePoint(),
})
@val
external fromCodePoint: int => t = "String.fromCodePoint"

/**
`fromCodePointMany([n1, n2, n3])` creates a `string` from the characters
corresponding to the given code point numbers, using the same rules as
`fromCodePoint`.

See [`String.fromCodePoint`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCodePoint)
on MDN.

## Examples

```rescript
Js.String2.fromCodePointMany([0xd55c, 0xae00, 0x1f63a]) == `한글😺`
```
*/
@deprecated({
  reason: "Use `String.fromCodePointMany` instead.",
  migrate: String.fromCodePointMany(),
})
@val
@variadic
external fromCodePointMany: array<int> => t = "String.fromCodePoint"

/* String.raw: ES2015, meant to be used with template strings, not directly */

/**
`length(s)` returns the length of the given `string`. See
[`String.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
on MDN.

## Examples

```rescript
Js.String2.length("abcd") == 4
```
*/
@deprecated({
  reason: "Use `String.length` instead.",
  migrate: String.length(),
})
@get
external length: t => int = "length"

/**
`get(s, n)` returns as a `string` the character at the given index number. If
`n` is out of range, this function returns `undefined`, so at some point this
function may be modified to return `option<string>`.

## Examples

```rescript
Js.String2.get("Reason", 0) == "R"
Js.String2.get("Reason", 4) == "o"
Js.String2.get(`Rẽasöń`, 5) == `ń`
```
*/
@deprecated({
  reason: "Use `String.get` instead.",
  migrate: String.get(),
})
@get_index
external get: (t, int) => t = ""

/**
`charAt(n, s)` gets the character at index `n` within string `s`. If `n` is
negative or greater than the length of `s`, it returns the empty string. If the
string contains characters outside the range \\u0000-\\uffff, it will return the
first 16-bit value at that position in the string.

See [`String.charAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
on MDN.

## Examples

```rescript
Js.String.charAt(0, "Reason") == "R"
Js.String.charAt(12, "Reason") == ""
Js.String.charAt(5, `Rẽasöń`) == `ń`
```
*/
@send
external charAt: (t, int) => t = "charAt"
let charAt = (arg1, obj) => charAt(obj, arg1)

/**
`charCodeAt(n, s)` returns the character code at position `n` in string `s`;
the result is in the range 0-65535, unlke `codePointAt`, so it will not work
correctly for characters with code points greater than or equal to 0x10000. The
return type is `float` because this function returns NaN if `n` is less than
zero or greater than the length of the string.

See [`String.charCodeAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt)
on MDN.

## Examples

```rescript
Js.String.charCodeAt(0, `😺`) == 0xd83d->Belt.Int.toFloat
Js.String.codePointAt(0, `😺`) == Some(0x1f63a)
```
*/
@send
external charCodeAt: (t, int) => float = "charCodeAt"
let charCodeAt = (arg1, obj) => charCodeAt(obj, arg1)

/**
`codePointAt(n, s)` returns the code point at position `n` within string `s` as
a `Some(value)`. The return value handles code points greater than or equal to
0x10000. If there is no code point at the given position, the function returns
`None`.

See [`String.codePointAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt)
on MDN.

## Examples

```rescript
Js.String.codePointAt(1, `¿😺?`) == Some(0x1f63a)
Js.String.codePointAt(5, "abc") == None
```
*/
@send
external codePointAt: (t, int) => option<int> = "codePointAt"
let codePointAt = (arg1, obj) => codePointAt(obj, arg1)

/**
`concat(append, original)` returns a new `string` with `append` added after
`original`.

See [`String.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
on MDN.

## Examples

```rescript
Js.String.concat("bell", "cow") == "cowbell"
```
*/
@send
external concat: (t, t) => t = "concat"
let concat = (arg1, obj) => concat(obj, arg1)

/**
`concat(arr, original)` returns a new `string` consisting of each item of an
array of strings added to the `original` string.

See [`String.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
on MDN.

## Examples

```rescript
Js.String.concatMany(["2nd", "3rd", "4th"], "1st") == "1st2nd3rd4th"
```
*/
@send @variadic
external concatMany: (t, array<t>) => t = "concat"
let concatMany = (arg1, obj) => concatMany(obj, arg1)

/**
ES2015: `endsWith(substr, str)` returns `true` if the `str` ends with `substr`,
`false` otherwise.

See [`String.endsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith)
on MDN.

## Examples

```rescript
Js.String.endsWith("Script", "ReScript") == true
Js.String.endsWith("Script", "C++") == false
```
*/
@send
external endsWith: (t, t) => bool = "endsWith"
let endsWith = (arg1, obj) => endsWith(obj, arg1)

/**
`endsWithFrom(ending, len, str)` returns `true` if the first len characters of
`str` end with `ending`, `false` otherwise. If `len` is greater than or equal
to the length of `str`, then it works like `endsWith`. (Honestly, this should
have been named endsWithAt, but oh well.)

See [`String.endsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith)
on MDN.

## Examples

```rescript
Js.String.endsWithFrom("cd", 4, "abcd") == true
Js.String.endsWithFrom("cd", 3, "abcde") == false
Js.String.endsWithFrom("cde", 99, "abcde") == true
Js.String.endsWithFrom("ple", 7, "example.dat") == true
```
*/
@send
external endsWithFrom: (t, t, int) => bool = "endsWith"
let endsWithFrom = (arg1, arg2, obj) => endsWithFrom(obj, arg1, arg2)

/**
ES2015: `includes(searchValue, str)` returns `true` if `searchValue` is found
anywhere within `str`, false otherwise.

See [`String.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes)
on MDN.

## Examples

```rescript
Js.String.includes("gram", "programmer") == true
Js.String.includes("er", "programmer") == true
Js.String.includes("pro", "programmer") == true
Js.String.includes("xyz", "programmer.dat") == false
```
*/
@send
external includes: (t, t) => bool = "includes"
let includes = (arg1, obj) => includes(obj, arg1)

/**
ES2015: `includes(searchValue start, str)` returns `true` if `searchValue` is
found anywhere within `str` starting at character number `start` (where 0 is
the first character), `false` otherwise.

See [`String.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes)
on MDN.

## Examples

```rescript
Js.String.includesFrom("gram", 1, "programmer") == true
Js.String.includesFrom("gram", 4, "programmer") == false
Js.String.includesFrom(`한`, 1, `대한민국`) == true
```
*/
@send
external includesFrom: (t, t, int) => bool = "includes"
let includesFrom = (arg1, arg2, obj) => includesFrom(obj, arg1, arg2)

/**
ES2015: `indexOf(searchValue, str)` returns the position at which `searchValue`
was first found within `str`, or -1 if `searchValue` is not in `str`.

See [`String.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
on MDN.

## Examples

```rescript
Js.String.indexOf("ok", "bookseller") == 2
Js.String.indexOf("sell", "bookseller") == 4
Js.String.indexOf("ee", "beekeeper") == 1
Js.String.indexOf("xyz", "bookseller") == -1
```
*/
@send
external indexOf: (t, t) => int = "indexOf"
let indexOf = (arg1, obj) => indexOf(obj, arg1)

/**
`indexOfFrom(searchValue, start, str)` returns the position at which
`searchValue` was found within `str` starting at character position `start`, or
\-1 if `searchValue` is not found in that portion of `str`. The return value is
relative to the beginning of the string, no matter where the search started
from.

See [`String.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
on MDN.

## Examples

```rescript
Js.String.indexOfFrom("ok", 1, "bookseller") == 2
Js.String.indexOfFrom("sell", 2, "bookseller") == 4
Js.String.indexOfFrom("sell", 5, "bookseller") == -1
```
*/
@send
external indexOfFrom: (t, t, int) => int = "indexOf"
let indexOfFrom = (arg1, arg2, obj) => indexOfFrom(obj, arg1, arg2)

/**
`lastIndexOf(searchValue, str)` returns the position of the last occurrence of
`searchValue` within `str`, searching backwards from the end of the string.
Returns -1 if `searchValue` is not in `str`. The return value is always
relative to the beginning of the string.

See [`String.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/lastIndexOf)
on MDN.

## Examples

```rescript
Js.String.lastIndexOf("ok", "bookseller") == 2
Js.String.lastIndexOf("ee", "beekeeper") == 4
Js.String.lastIndexOf("xyz", "abcdefg") == -1
```
*/
@send
external lastIndexOf: (t, t) => int = "lastIndexOf"
let lastIndexOf = (arg1, obj) => lastIndexOf(obj, arg1)

/**
`lastIndexOfFrom(searchValue, start, str)` returns the position of the last
occurrence of `searchValue` within `str`, searching backwards from the given
start position. Returns -1 if `searchValue` is not in `str`. The return value
is always relative to the beginning of the string.

See [`String.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/lastIndexOf)
on MDN.

## Examples

```rescript
Js.String.lastIndexOfFrom("ok", 6, "bookseller") == 2
Js.String.lastIndexOfFrom("ee", 8, "beekeeper") == 4
Js.String.lastIndexOfFrom("ee", 3, "beekeeper") == 1
Js.String.lastIndexOfFrom("xyz", 4, "abcdefg") == -1
```
*/
@send
external lastIndexOfFrom: (t, t, int) => int = "lastIndexOf"
let lastIndexOfFrom = (arg1, arg2, obj) => lastIndexOfFrom(obj, arg1, arg2)

/* extended by ECMA-402 */

/**
`localeCompare(comparison, reference)` returns
- a negative value if reference comes before comparison in sort order
- zero if reference and comparison have the same sort order
- a positive value if reference comes after comparison in sort order

See [`String.localeCompare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare) on MDN.

## Examples

```rescript
Js.String.localeCompare("ant", "zebra") > 0.0
Js.String.localeCompare("zebra", "ant") < 0.0
Js.String.localeCompare("cat", "cat") == 0.0
Js.String.localeCompare("cat", "CAT") > 0.0
```
*/
@send
external localeCompare: (t, t) => float = "localeCompare"
let localeCompare = (arg1, obj) => localeCompare(obj, arg1)

/**
`match(regexp, str)` matches a `string` against the given `regexp`. If there is
no match, it returns `None`. For regular expressions without the g modifier, if
there is a match, the return value is `Some(array)` where the array contains:
- The entire matched string
- Any capture groups if the regexp had parentheses

For regular expressions with the g modifier, a matched expression returns
`Some(array)` with all the matched substrings and no capture groups.

See [`String.match`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)
on MDN.

## Examples

```rescript
Js.String.match_(/b[aeiou]t/, "The better bats") == Some(["bet"])
Js.String.match_(/b[aeiou]t/g, "The better bats") == Some(["bet", "bat"])
Js.String.match_(/(\d+)-(\d+)-(\d+)/, "Today is 2018-04-05.") ==
  Some(["2018-04-05", "2018", "04", "05"])
Js.String.match_(/b[aeiou]g/, "The large container.") == None
```
*/
@send @return(null_to_opt)
external match_: (t, Js_re.t) => option<array<option<t>>> = "match"
let match_ = (arg1, obj) => match_(obj, arg1)

/**
`normalize(str)` returns the normalized Unicode string using Normalization Form
Canonical (NFC) Composition. Consider the character ã, which can be represented
as the single codepoint \u00e3 or the combination of a lower case letter A
\u0061 and a combining tilde \u0303. Normalization ensures that both can be
stored in an equivalent binary representation.

See [`String.normalize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/normalize)
on MDN.

See also [Unicode technical report #15](https://unicode.org/reports/tr15/) for
details.
*/
@deprecated({
  reason: "Use `String.normalize` instead.",
  migrate: String.normalize(),
})
@send
external normalize: t => t = "normalize"

/**
ES2015: `normalize(form, str)` returns the normalized Unicode string using the specified form of normalization, which may be one of:
- "NFC" — Normalization Form Canonical Composition.
- "NFD" — Normalization Form Canonical Decomposition.
- "NFKC" — Normalization Form Compatibility Composition.
- "NFKD" — Normalization Form Compatibility Decomposition.

See [`String.normalize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/normalize) on MDN.

See also [Unicode technical report #15](https://unicode.org/reports/tr15/) for details.
*/
@send
external normalizeByForm: (t, t) => t = "normalize"
let normalizeByForm = (arg1, obj) => normalizeByForm(obj, arg1)

/**
`repeat(n, str)` returns a `string` that consists of `n` repetitions of `str`.
Throws `RangeError` if `n` is negative.

See [`String.repeat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/repeat)
on MDN.

## Examples

```rescript
Js.String.repeat(3, "ha") == "hahaha"
Js.String.repeat(0, "empty") == ""
```
*/
@send
external repeat: (t, int) => t = "repeat"
let repeat = (arg1, obj) => repeat(obj, arg1)

/**
ES2015: `replace(substr, newSubstr, str)` returns a new `string` which is
identical to `str` except with the first matching instance of `substr` replaced
by `newSubstr`. `substr` is treated as a verbatim string to match, not a
regular expression.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
Js.String.replace("old", "new", "old string") == "new string"
Js.String.replace("the", "this", "the cat and the dog") == "this cat and the dog"
```
*/
@send
external replace: (t, t, t) => t = "replace"
let replace = (arg1, arg2, obj) => replace(obj, arg1, arg2)

/**
`replaceByRe(regex, replacement, str)` returns a new `string` where occurrences
matching regex have been replaced by `replacement`.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
Js.String.replaceByRe(/[aeiou]/g, "x", "vowels be gone") == "vxwxls bx gxnx"
Js.String.replaceByRe(/(\w+) (\w+)/, "$2, $1", "Juan Fulano") == "Fulano, Juan"
```
*/
@send
external replaceByRe: (t, Js_re.t, t) => t = "replace"
let replaceByRe = (arg1, arg2, obj) => replaceByRe(obj, arg1, arg2)

/**
Returns a new `string` with some or all matches of a pattern with no capturing
parentheses replaced by the value returned from the given function. The
function receives as its parameters the matched string, the offset at which the
match begins, and the whole string being matched.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
let str = "beautiful vowels"
let re = /[aeiou]/g
let matchFn = (matchPart, _offset, _wholeString) => Js.String.toUpperCase(matchPart)

Js.String.unsafeReplaceBy0(re, matchFn, str) == "bEAUtIfUl vOwEls"
```
*/
@send
external unsafeReplaceBy0: (t, Js_re.t, (t, int, t) => t) => t = "replace"
let unsafeReplaceBy0 = (arg1, arg2, obj) => unsafeReplaceBy0(obj, arg1, arg2)

/**
Returns a new `string` with some or all matches of a pattern with one set of
capturing parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured
string, the offset at which the match begins, and the whole string being
matched.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
let str = "Jony is 40"
let re = /(Jony is )\d+/g
let matchFn = (_match, part1, _offset, _wholeString) => {
  part1 ++ "41"
}

Js.String.unsafeReplaceBy1(re, matchFn, str) == "Jony is 41"
```
*/
@send
external unsafeReplaceBy1: (t, Js_re.t, (t, t, int, t) => t) => t = "replace"
let unsafeReplaceBy1 = (arg1, arg2, obj) => unsafeReplaceBy1(obj, arg1, arg2)

/**
Returns a new `string` with some or all matches of a pattern with two sets of
capturing parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured
strings, the offset at which the match begins, and the whole string being
matched.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
let str = "7 times 6"
let re = /(\d+) times (\d+)/
let matchFn = (_match, p1, p2, _offset, _wholeString) => {
  switch (Belt.Int.fromString(p1), Belt.Int.fromString(p2)) {
  | (Some(x), Some(y)) => Belt.Int.toString(x * y)
  | _ => "???"
  }
}

Js.String.unsafeReplaceBy2(re, matchFn, str) == "42"
```
*/
@send
external unsafeReplaceBy2: (t, Js_re.t, (t, t, t, int, t) => t) => t = "replace"
let unsafeReplaceBy2 = (arg1, arg2, obj) => unsafeReplaceBy2(obj, arg1, arg2)

/**
Returns a new `string` with some or all matches of a pattern with three sets of
capturing parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured
strings, the offset at which the match begins, and the whole string being
matched.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.
*/
@send
external unsafeReplaceBy3: (t, Js_re.t, (t, t, t, t, int, t) => t) => t = "replace"
let unsafeReplaceBy3 = (arg1, arg2, obj) => unsafeReplaceBy3(obj, arg1, arg2)

/**
`search(regexp, str)` returns the starting position of the first match of
`regexp` in the given `str`, or -1 if there is no match.

See [`String.search`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/search)
on MDN.

## Examples

```rescript
Js.String.search(/\d+/, "testing 1 2 3") == 8
Js.String.search(/\d+/, "no numbers") == -1
```
*/
@send
external search: (t, Js_re.t) => int = "search"
let search = (arg1, obj) => search(obj, arg1)

/**
`slice(from:n1, to_:n2, str)` returns the substring of `str` starting at
character `n1` up to but not including `n2`.
- If either `n1` or `n2` is negative, then it is evaluated as `length(str - n1)` or `length(str - n2)`.
- If `n2` is greater than the length of `str`, then it is treated as `length(str)`.
- If `n1` is greater than `n2`, slice returns the empty string.

See [`String.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice) on MDN.

## Examples

```rescript
Js.String.slice(~from=2, ~to_=5, "abcdefg") == "cde"
Js.String.slice(~from=2, ~to_=9, "abcdefg") == "cdefg"
Js.String.slice(~from=-4, ~to_=-2, "abcdefg") == "de"
Js.String.slice(~from=5, ~to_=1, "abcdefg") == ""
```
*/
@send
external slice: (t, ~from: int, ~to_: int) => t = "slice"
let slice = (~from, ~to_, obj) => slice(obj, ~from, ~to_)

/**
`sliceToEnd(str, from:n)` returns the substring of `str` starting at character
`n` to the end of the string.
- If `n` is negative, then it is evaluated as `length(str - n)`.
- If `n` is greater than the length of `str`, then sliceToEnd returns the empty string.

See [`String.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice) on MDN.

## Examples

```rescript
Js.String.sliceToEnd(~from=4, "abcdefg") == "efg"
Js.String.sliceToEnd(~from=-2, "abcdefg") == "fg"
Js.String.sliceToEnd(~from=7, "abcdefg") == ""
```
*/
@send
external sliceToEnd: (t, ~from: int) => t = "slice"
let sliceToEnd = (~from, obj) => sliceToEnd(obj, ~from)

/**
`split(delimiter, str)` splits the given `str` at every occurrence of
`delimiter` and returns an array of the resulting substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String.split("-", "2018-01-02") == ["2018", "01", "02"]
Js.String.split(",", "a,b,,c") == ["a", "b", "", "c"]
Js.String.split("::", "good::bad as great::awful") == ["good", "bad as great", "awful"]
Js.String.split(";", "has-no-delimiter") == ["has-no-delimiter"]
```
*/
@send
external split: (t, t) => array<t> = "split"
let split = (arg1, obj) => split(obj, arg1)

/**
`splitAtMost(delimiter, ~limit:n, str)` splits the given `str` at every
occurrence of `delimiter` and returns an array of the first `n` resulting
substrings. If `n` is negative or greater than the number of substrings, the
array will contain all the substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String.splitAtMost("/", ~limit=3, "ant/bee/cat/dog/elk") == ["ant", "bee", "cat"]
Js.String.splitAtMost("/", ~limit=0, "ant/bee/cat/dog/elk") == []
Js.String.splitAtMost("/", ~limit=9, "ant/bee/cat/dog/elk") == ["ant", "bee", "cat", "dog", "elk"]
```
*/
@send
external splitAtMost: (t, t, ~limit: int) => array<t> = "split"
let splitAtMost = (arg1, ~limit, obj) => splitAtMost(obj, ~limit, arg1)

/**
`splitByRe(regex, str)` splits the given `str` at every occurrence of `regex`
and returns an array of the resulting substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String.splitByRe(/\s*[,;]\s*TODO/, "art; bed , cog ;dad") == [
    Some("art"),
    Some("bed"),
    Some("cog"),
    Some("dad"),
  ]
```
*/
@send
external splitByRe: (t, Js_re.t) => array<option<t>> = "split"
let splitByRe = (arg1, obj) => splitByRe(obj, arg1)

/**
`splitByReAtMost(regex, ~limit:n, str)` splits the given `str` at every
occurrence of `regex` and returns an array of the first `n` resulting
substrings. If `n` is negative or greater than the number of substrings, the
array will contain all the substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String.splitByReAtMost(/\s*[,;]\s*TODO/, ~limit=3, "one: two: three: four") == [
    Some("one"),
    Some("two"),
    Some("three"),
  ]

Js.String.splitByReAtMost(/\s*[,;]\s*TODO/, ~limit=0, "one: two: three: four") == []

Js.String.splitByReAtMost(/\s*[,;]\s*TODO/, ~limit=8, "one: two: three: four") == [
    Some("one"),
    Some("two"),
    Some("three"),
    Some("four"),
  ]
```
*/
@send
external splitByReAtMost: (t, Js_re.t, ~limit: int) => array<option<t>> = "split"
let splitByReAtMost = (arg1, ~limit, obj) => splitByReAtMost(obj, arg1, ~limit)

/**
ES2015: `startsWith(substr, str)` returns `true` if the `str` starts with
`substr`, `false` otherwise.

See [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith)
on MDN.

## Examples

```rescript
Js.String.startsWith("Re", "ReScript") == true
Js.String.startsWith("", "ReScript") == true
Js.String.startsWith("Re", "JavaScript") == false
```
*/
@send
external startsWith: (t, t) => bool = "startsWith"
let startsWith = (arg1, obj) => startsWith(obj, arg1)

/**
ES2015: `startsWithFrom(substr, n, str)` returns `true` if the `str` starts
with `substr` starting at position `n`, false otherwise. If `n` is negative,
the search starts at the beginning of `str`.

See [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith)
on MDN.

## Examples

```rescript
Js.String.startsWithFrom("Scri", 2, "ReScript") == true
Js.String.startsWithFrom("", 2, "ReScript") == true
Js.String.startsWithFrom("Scri", 2, "JavaScript") == false
```
*/
@send
external startsWithFrom: (t, t, int) => bool = "startsWith"
let startsWithFrom = (arg1, arg2, obj) => startsWithFrom(obj, arg1, arg2)

/**
`substr(~from:n, str)` returns the substring of `str` from position `n` to the
end of the string.
- If `n` is less than zero, the starting position is the length of `str - n`.
- If `n` is greater than or equal to the length of `str`, returns the empty string.

JavaScript’s `String.substr()` is a legacy function. When possible, use
`substring()` instead.

See [`String.substr`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
on MDN.

## Examples

```rescript
Js.String.substr(~from=3, "abcdefghij") == "defghij"
Js.String.substr(~from=-3, "abcdefghij") == "hij"
Js.String.substr(~from=12, "abcdefghij") == ""
```
*/
@send
external substr: (t, ~from: int) => t = "substr"
let substr = (~from, obj) => substr(obj, ~from)

/**
`substrAtMost(~from: pos, ~length: n, str)` returns the substring of `str` of
length `n` starting at position `pos`.
- If `pos` is less than zero, the starting position is the length of `str - pos`.
- If `pos` is greater than or equal to the length of `str`, returns the empty string.
- If `n` is less than or equal to zero, returns the empty string.

JavaScript’s `String.substr()` is a legacy function. When possible, use
`substring()` instead.

See [`String.substr`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
on MDN.

## Examples

```rescript
Js.String.substrAtMost(~from=3, ~length=4, "abcdefghij") == "defg"
Js.String.substrAtMost(~from=-3, ~length=4, "abcdefghij") == "hij"
Js.String.substrAtMost(~from=12, ~length=2, "abcdefghij") == ""
```
*/
@send
external substrAtMost: (t, ~from: int, ~length: int) => t = "substr"
let substrAtMost = (~from, ~length, obj) => substrAtMost(obj, ~from, ~length)

/**
`substring(~from: start, ~to_: finish, str)` returns characters `start` up to
but not including finish from `str`.
- If `start` is less than zero, it is treated as zero.
- If `finish` is zero or negative, the empty string is returned.
- If `start` is greater than `finish`, the `start` and `finish` points are swapped.

See [`String.substring`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring) on MDN.

## Examples

```rescript
Js.String.substring(~from=3, ~to_=6, "playground") == "ygr"
Js.String.substring(~from=6, ~to_=3, "playground") == "ygr"
Js.String.substring(~from=4, ~to_=12, "playground") == "ground"
```
*/
@send
external substring: (t, ~from: int, ~to_: int) => t = "substring"
let substring = (~from, ~to_, obj) => substring(obj, ~from, ~to_)

/**
`substringToEnd(~from: start, str)` returns the substring of `str` from
position `start` to the end.
- If `start` is less than or equal to zero, the entire string is returned.
- If `start` is greater than or equal to the length of `str`, the empty string is returned.

See [`String.substring`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring) on MDN.

## Examples

```rescript
Js.String.substringToEnd(~from=4, "playground") == "ground"
Js.String.substringToEnd(~from=-3, "playground") == "playground"
Js.String.substringToEnd(~from=12, "playground") == ""
```
*/
@send
external substringToEnd: (t, ~from: int) => t = "substring"
let substringToEnd = (~from, obj) => substringToEnd(obj, ~from)

/**
`toLowerCase(str)` converts `str` to lower case using the locale-insensitive
case mappings in the Unicode Character Database. Notice that the conversion can
give different results depending upon context, for example with the Greek
letter sigma, which has two different lower case forms; one when it is the last
character in a string and another when it is not.

See [`String.toLowerCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
on MDN.

## Examples

```rescript
Js.String.toLowerCase("ABC") == "abc"
Js.String.toLowerCase(`ΣΠ`) == `σπ`
Js.String.toLowerCase(`ΠΣ`) == `πς`
```
*/
@deprecated({
  reason: "Use `String.toLowerCase` instead.",
  migrate: String.toLowerCase(),
})
@send
external toLowerCase: t => t = "toLowerCase"

/**
`toLocaleLowerCase(str)` converts `str` to lower case using the current locale.

See [`String.toLocaleLowerCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
on MDN.
*/
@deprecated({
  reason: "Use `String.toLocaleLowerCase` instead.",
  migrate: String.toLocaleLowerCase(),
})
@send
external toLocaleLowerCase: t => t = "toLocaleLowerCase"

/**
`toUpperCase(str)` converts `str` to upper case using the locale-insensitive
case mappings in the Unicode Character Database. Notice that the conversion can
expand the number of letters in the result; for example the German ß
capitalizes to two Ses in a row.

See [`String.toUpperCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
on MDN.

## Examples

```rescript
Js.String.toUpperCase("abc") == "ABC"
Js.String.toUpperCase(`Straße`) == `STRASSE`
Js.String.toUpperCase(`πς`) == `ΠΣ`
```
*/
@deprecated({
  reason: "Use `String.toUpperCase` instead.",
  migrate: String.toUpperCase(),
})
@send
external toUpperCase: t => t = "toUpperCase"

/**
`toLocaleUpperCase(str)` converts `str` to upper case using the current locale.

See [`String.to:LocaleUpperCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
on MDN.
*/
@deprecated({
  reason: "Use `String.toLocaleUpperCase` instead.",
  migrate: String.toLocaleUpperCase(),
})
@send
external toLocaleUpperCase: t => t = "toLocaleUpperCase"

/**
`trim(str)` returns a string that is `str` with whitespace stripped from both
ends. Internal whitespace is not removed.

See [`String.trim`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/trim)
on MDN.

## Examples

```rescript
Js.String.trim("   abc def   ") == "abc def"
Js.String.trim("\n\r\t abc def \n\n\t\r ") == "abc def"
```
*/
@deprecated({
  reason: "Use `String.trim` instead.",
  migrate: String.trim(),
})
@send
external trim: t => t = "trim"

/* HTML wrappers */

/**
`anchor(anchorName, anchorText)` creates a string with an HTML `<a>` element
with name attribute of `anchorName` and `anchorText` as its content. Please do
not use this method, as it has been removed from the relevant web standards.

See [`String.anchor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/anchor)
on MDN.

## Examples

```rescript
Js.String.anchor("page1", "Page One") == "<a name=\"page1\">Page One</a>"
```
*/
@send
external anchor: (t, t) => t = "anchor"
let anchor = (arg1, obj) => anchor(obj, arg1)

/**
ES2015: `link(urlText, linkText)` creates a string with an HTML `<a>` element
with href attribute of `urlText` and `linkText` as its content. Please do not
use this method, as it has been removed from the relevant web standards.

See [`String.link`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/link)
on MDN.

## Examples

```rescript
Js.String.link("page2.html", "Go to page two") == "<a href=\"page2.html\">Go to page two</a>"
```
*/
@send
external link: (t, t) => t = "link"
let link = (arg1, obj) => link(obj, arg1)

/**
Casts its argument to an `array_like` entity that can be processed by functions
such as `Js.Array2.fromMap()`

## Examples

```rescript
let s = "abcde"
let arr = Js.Array2.fromMap(Js.String.castToArrayLike(s), x => x)
arr == ["a", "b", "c", "d", "e"]
```
*/
@deprecated(
  "This has been deprecated and will be removed in v13. Use functions from the `String` module instead."
)
external castToArrayLike: t => Js_array2.array_like<t> = "%identity"
