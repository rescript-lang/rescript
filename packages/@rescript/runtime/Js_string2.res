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

/*** Provide bindings to JS string. Optimized for pipe-first. */

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
  reason: "Use `String.make` instead",
  migrate: String.make(),
})
@val
external make: 'a => t = "String"

/**
`fromCharCode(n)` creates a `string` containing the character corresponding to
that number; `n` ranges from 0 to 65535.If out of range, the lower 16 bits of
the value are used. Thus, `fromCharCode(0x1F63A)` gives the same result as
`fromCharCode(0xF63A)`.

See [`String.fromCharCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode)
on MDN.

## Examples

```rescript
Js.String2.fromCharCode(65) == "A"
Js.String2.fromCharCode(0x3c8) == `ψ`
Js.String2.fromCharCode(0xd55c) == `한`
Js.String2.fromCharCode(-64568) == `ψ`
```
*/
@deprecated({
  reason: "Use `String.fromCharCode` instead",
  migrate: String.fromCharCode(),
})
@val
external fromCharCode: int => t = "String.fromCharCode"

/**
`fromCharCodeMany([n1, n2, n3])` creates a `string` from the characters
corresponding to the given numbers, using the same rules as `fromCharCode`.

See [`String.fromCharCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode)
on MDN.
*/
@deprecated({
  reason: "Use `String.fromCharCodeMany` instead",
  migrate: String.fromCharCodeMany(),
})
@val
@variadic
external fromCharCodeMany: array<int> => t = "String.fromCharCode"

/**
`fromCodePoint(n)` creates a `string` containing the character corresponding to
that numeric code point. If the number is not a valid code point, it throws
`RangeError`. Thus, `fromCodePoint(0x1F63A)` will produce a correct value,
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
  reason: "Use `String.fromCodePoint` instead",
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
  reason: "Use `String.fromCodePointMany` instead",
  migrate: String.fromCodePointMany(),
})
@val
@variadic
external fromCodePointMany: array<int> => t = "String.fromCodePoint"

/* String.raw: ES2015, meant to be used with template strings, not directly */

/**
`length(s)` returns the length of the given `string`.

See [`String.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
on MDN.

## Examples

```rescript
Js.String2.length("abcd") == 4
```
*/
@deprecated({
  reason: "Use `String.length` instead",
  migrate: String.length(),
})
@get
external length: t => int = "length"

/**
`get(s, n)` returns as a `string` the character at the given index number. If
`n` is out of range, this function returns `undefined`,so at some point this
function may be modified to return `option<string>`.

## Examples

```rescript
Js.String2.get("Reason", 0) == "R"
Js.String2.get("Reason", 4) == "o"
Js.String2.get(`Rẽasöń`, 5) == `ń`
```
*/
@deprecated({
  reason: "Use `String.getUnsafe` instead. Or use `String.get` for a safe version that returns an option.",
  migrate: String.getUnsafe(),
})
@get_index
external get: (t, int) => t = ""

/**
`charAt(s, n)` gets the character at index `n` within string `s`. If `n` is
negative or greater than the length of `s`, it returns the empty string. If the
string contains characters outside the range \\u0000-\\uffff, it will return the
first 16-bit value at that position in the string.

See [`String.charAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
on MDN.

## Examples

```rescript
Js.String2.charAt("Reason", 0) == "R"
Js.String2.charAt("Reason", 12) == ""
Js.String2.charAt(`Rẽasöń`, 5) == `ń`
```
*/
@deprecated({
  reason: "Use `String.charAt` instead",
  migrate: String.charAt(),
})
@send
external charAt: (t, int) => t = "charAt"

/**
`charCodeAt(s, n)` returns the character code at position `n` in string `s`;
the result is in the range 0-65535, unlke `codePointAt`, so it will not work
correctly for characters with code points greater than or equal to 0x10000. The
return type is `float` because this function returns NaN if `n` is less than
zero or greater than the length of the string.

See [`String.charCodeAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt)
on MDN.

## Examples

```rescript
Js.String2.charCodeAt(`😺`, 0) == 0xd83d->Belt.Int.toFloat
Js.String2.codePointAt(`😺`, 0) == Some(0x1f63a)
```
*/
@deprecated({
  reason: "Use `String.charCodeAt` instead",
  migrate: String.charCodeAt(),
})
@send
external charCodeAt: (t, int) => float = "charCodeAt"

/**
`codePointAt(s, n)` returns the code point at position `n` within string `s` as
a `Some(value)`. The return value handles code points greater than or equal to
0x10000. If there is no code point at the given position, the function returns
`None`.

See [`String.codePointAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt)
on MDN.

## Examples

```rescript
Js.String2.codePointAt(`¿😺?`, 1) == Some(0x1f63a)
Js.String2.codePointAt("abc", 5) == None
```
*/
@deprecated({
  reason: "Use `String.codePointAt` instead",
  migrate: String.codePointAt(),
})
@send
external codePointAt: (t, int) => option<int> = "codePointAt"

/**
`concat(original, append)` returns a new `string` with `append` added after
`original`.

See [`String.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
on MDN.

## Examples

```rescript
Js.String2.concat("cow", "bell") == "cowbell"
```
*/
@deprecated({
  reason: "Use `String.concat` instead",
  migrate: String.concat(),
})
@send
external concat: (t, t) => t = "concat"

/**
`concatMany(original, arr)` returns a new `string` consisting of each item of an
array of strings added to the `original` string.

See [`String.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
on MDN.

## Examples

```rescript
Js.String2.concatMany("1st", ["2nd", "3rd", "4th"]) == "1st2nd3rd4th"
```
*/
@deprecated({
  reason: "Use `String.concatMany` instead",
  migrate: String.concatMany(),
})
@send
@variadic
external concatMany: (t, array<t>) => t = "concat"

/**
ES2015: `endsWith(str, substr)` returns `true` if the `str` ends with `substr`,
`false` otherwise.

See [`String.endsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith)
on MDN.

## Examples

```rescript
Js.String2.endsWith("ReScript", "Script") == true
Js.String2.endsWith("C++", "Script") == false
```
*/
@deprecated({
  reason: "Use `String.endsWith` instead",
  migrate: String.endsWith(),
})
@send
external endsWith: (t, t) => bool = "endsWith"

/**
`endsWithFrom(str, ending, len)` returns `true` if the first len characters of
`str` end with `ending`, `false` otherwise. If `len` is greater than or equal
to the length of `str`, then it works like `endsWith`. (Honestly, this should
have been named endsWithAt, but oh well).

See [`String.endsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith)
on MDN.

## Examples

```rescript
Js.String2.endsWithFrom("abcd", "cd", 4) == true
Js.String2.endsWithFrom("abcde", "cd", 3) == false
Js.String2.endsWithFrom("abcde", "cde", 99) == true
Js.String2.endsWithFrom("example.dat", "ple", 7) == true
```
*/
@deprecated({
  reason: "Use `String.endsWithFrom` instead",
  migrate: String.endsWithFrom(),
})
@send
external endsWithFrom: (t, t, int) => bool = "endsWith"

/**
ES2015: `includes(str, searchValue)` returns `true` if `searchValue` is found
anywhere within `str`, false otherwise.

See [`String.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes)
on MDN.

## Examples

```rescript
Js.String2.includes("programmer", "gram") == true
Js.String2.includes("programmer", "er") == true
Js.String2.includes("programmer", "pro") == true
Js.String2.includes("programmer.dat", "xyz") == false
```
*/
@deprecated({
  reason: "Use `String.includes` instead",
  migrate: String.includes(),
})
@send
external includes: (t, t) => bool = "includes"

/**
ES2015: `includes(str, searchValue start)` returns `true` if `searchValue` is
found anywhere within `str` starting at character number `start` (where 0 is
the first character), `false` otherwise.

See [`String.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes)
on MDN.

## Examples

```rescript
Js.String2.includesFrom("programmer", "gram", 1) == true
Js.String2.includesFrom("programmer", "gram", 4) == false
Js.String2.includesFrom(`대한민국`, `한`, 1) == true
```
*/
@deprecated({
  reason: "Use `String.includesFrom` instead",
  migrate: String.includesFrom(),
})
@send
external includesFrom: (t, t, int) => bool = "includes"

/**
ES2015: `indexOf(str, searchValue)` returns the position at which `searchValue`
was first found within `str`, or -1 if `searchValue` is not in `str`.

See [`String.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
on MDN.

## Examples

```rescript
Js.String2.indexOf("bookseller", "ok") == 2
Js.String2.indexOf("bookseller", "sell") == 4
Js.String2.indexOf("beekeeper", "ee") == 1
Js.String2.indexOf("bookseller", "xyz") == -1
```
*/
@deprecated({
  reason: "Use `String.indexOf` instead",
  migrate: String.indexOf(),
})
@send
external indexOf: (t, t) => int = "indexOf"

/**
`indexOfFrom(str, searchValue, start)` returns the position at which
`searchValue` was found within `str` starting at character position `start`, or
\-1 if `searchValue` is not found in that portion of `str`. The return value is
relative to the beginning of the string, no matter where the search started
from.

See [`String.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
on MDN.

## Examples

```rescript
Js.String2.indexOfFrom("bookseller", "ok", 1) == 2
Js.String2.indexOfFrom("bookseller", "sell", 2) == 4
Js.String2.indexOfFrom("bookseller", "sell", 5) == -1
```
*/
@deprecated({
  reason: "Use `String.indexOfFrom` instead",
  migrate: String.indexOfFrom(),
})
@send
external indexOfFrom: (t, t, int) => int = "indexOf"

/**
`lastIndexOf(str, searchValue)` returns the position of the last occurrence of
`searchValue` within `str`, searching backwards from the end of the string.
Returns -1 if `searchValue` is not in `str`. The return value is always
relative to the beginning of the string.

See [`String.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/lastIndexOf)
on MDN.

## Examples

```rescript
Js.String2.lastIndexOf("bookseller", "ok") == 2
Js.String2.lastIndexOf("beekeeper", "ee") == 4
Js.String2.lastIndexOf("abcdefg", "xyz") == -1
```
*/
@deprecated({
  reason: "Use `String.lastIndexOf` instead",
  migrate: String.lastIndexOf(),
})
@send
external lastIndexOf: (t, t) => int = "lastIndexOf"

/**
`lastIndexOfFrom(str, searchValue, start)` returns the position of the last
occurrence of `searchValue` within `str`, searching backwards from the given
start position. Returns -1 if `searchValue` is not in `str`. The return value
is always relative to the beginning of the string.

See [`String.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/lastIndexOf)
on MDN.

## Examples

```rescript
Js.String2.lastIndexOfFrom("bookseller", "ok", 6) == 2
Js.String2.lastIndexOfFrom("beekeeper", "ee", 8) == 4
Js.String2.lastIndexOfFrom("beekeeper", "ee", 3) == 1
Js.String2.lastIndexOfFrom("abcdefg", "xyz", 4) == -1
```
*/
@deprecated({
  reason: "Use `String.lastIndexOfFrom` instead",
  migrate: String.lastIndexOfFrom(),
})
@send
external lastIndexOfFrom: (t, t, int) => int = "lastIndexOf"

/* extended by ECMA-402 */

/**
`localeCompare(reference, comparison)` returns
- a negative value if reference comes before comparison in sort order
- zero if reference and comparison have the same sort order
- a positive value if reference comes after comparison in sort order

See [`String.localeCompare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare) on MDN.

## Examples

```rescript
Js.String2.localeCompare("zebra", "ant") > 0.0
Js.String2.localeCompare("ant", "zebra") < 0.0
Js.String2.localeCompare("cat", "cat") == 0.0
Js.String2.localeCompare("CAT", "cat") > 0.0
```
*/
@deprecated({
  reason: "Use `String.localeCompare` instead",
  migrate: String.localeCompare(),
})
@send
external localeCompare: (t, t) => float = "localeCompare"

/**
`match(str, regexp)` matches a `string` against the given `regexp`. If there is
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
Js.String2.match_("The better bats", /b[aeiou]t/) == Some(["bet"])
Js.String2.match_("The better bats", /b[aeiou]t/g) == Some(["bet", "bat"])
Js.String2.match_("Today is 2018-04-05.", /(\d+)-(\d+)-(\d+)/) ==
  Some(["2018-04-05", "2018", "04", "05"])
Js.String2.match_("The large container.", /b[aeiou]g/) == None
```
*/
@deprecated({
  reason: "Use `String.match` instead",
  migrate: String.match(),
})
@send
@return({null_to_opt: null_to_opt})
external match_: (t, Js_re.t) => option<array<option<t>>> = "match"

/**
`normalize(str)` returns the normalized Unicode string using Normalization Form
Canonical (NFC) Composition. Consider the character ã, which can be represented
as the single codepoint \u00e3 or the combination of a lower case letter A
\u0061 and a combining tilde \u0303. Normalization ensures that both can be
stored in an equivalent binary representation.

See [`String.normalize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/normalize)
on MDN. See also [Unicode technical report
#15](https://unicode.org/reports/tr15/) for details.
*/
@deprecated({
  reason: "Use `String.normalize` instead",
  migrate: String.normalize(),
})
@send
external normalize: t => t = "normalize"

/**
ES2015: `normalize(str, form)` returns the normalized Unicode string using the
specified form of normalization, which may be one of:
- "NFC" — Normalization Form Canonical Composition.
- "NFD" — Normalization Form Canonical Decomposition.
- "NFKC" — Normalization Form Compatibility Composition.
- "NFKD" — Normalization Form Compatibility Decomposition.

See [`String.normalize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/normalize) on MDN.
See also [Unicode technical report #15](https://unicode.org/reports/tr15/) for details.
*/
@deprecated({
  reason: "Use `String.normalizeByForm` instead",
  migrate: String.normalizeByForm(),
})
@send
external normalizeByForm: (t, t) => t = "normalize"

/**
`repeat(str, n)` returns a `string` that consists of `n` repetitions of `str`.
Throws `RangeError` if `n` is negative.

See [`String.repeat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/repeat)
on MDN.

## Examples

```rescript
Js.String2.repeat("ha", 3) == "hahaha"
Js.String2.repeat("empty", 0) == ""
```
*/
@deprecated({
  reason: "Use `String.repeat` instead",
  migrate: String.repeat(),
})
@send
external repeat: (t, int) => t = "repeat"

/**
ES2015: `replace(str, substr, newSubstr)` returns a new `string` which is
identical to `str` except with the first matching instance of `substr` replaced
by `newSubstr`. `substr` is treated as a verbatim string to match, not a
regular expression.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
Js.String2.replace("old string", "old", "new") == "new string"
Js.String2.replace("the cat and the dog", "the", "this") == "this cat and the dog"
```
*/
@deprecated({
  reason: "Use `String.replace` instead",
  migrate: String.replace(),
})
@send
external replace: (t, t, t) => t = "replace"

/**
`replaceByRe(str, regex, replacement)` returns a new `string` where occurrences
matching regex have been replaced by `replacement`.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.

## Examples

```rescript
Js.String2.replaceByRe("vowels be gone", /[aeiou]/g, "x") == "vxwxls bx gxnx"
Js.String2.replaceByRe("Juan Fulano", /(\w+) (\w+)/, "$2, $1") == "Fulano, Juan"
```
*/
@deprecated({
  reason: "Use `String.replaceRegExp` instead",
  migrate: String.replaceRegExp(),
})
@send
external replaceByRe: (t, Js_re.t, t) => t = "replace"

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
let matchFn = (matchPart, _offset, _wholeString) => Js.String2.toUpperCase(matchPart)

Js.String2.unsafeReplaceBy0(str, re, matchFn) == "bEAUtIfUl vOwEls"
```
*/
@deprecated({
  reason: "Use `String.replaceRegExpBy0Unsafe` instead",
  migrate: String.replaceRegExpBy0Unsafe(),
})
@send
external unsafeReplaceBy0: (t, Js_re.t, (t, int, t) => t) => t = "replace"

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

Js.String2.unsafeReplaceBy1(str, re, matchFn) == "Jony is 41"
```
*/
@deprecated({
  reason: "Use `String.replaceRegExpBy1Unsafe` instead",
  migrate: String.replaceRegExpBy1Unsafe(),
})
@send
external unsafeReplaceBy1: (t, Js_re.t, (t, t, int, t) => t) => t = "replace"

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

Js.String2.unsafeReplaceBy2(str, re, matchFn) == "42"
```
*/
@deprecated({
  reason: "Use `String.replaceRegExpBy2Unsafe` instead",
  migrate: String.replaceRegExpBy2Unsafe(),
})
@send
external unsafeReplaceBy2: (t, Js_re.t, (t, t, t, int, t) => t) => t = "replace"

/**
Returns a new `string` with some or all matches of a pattern with three sets of
capturing parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured
strings, the offset at which the match begins, and the whole string being
matched.

See [`String.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
on MDN.
*/
@deprecated({
  reason: "Use `String.replaceRegExpBy3Unsafe` instead",
  migrate: String.replaceRegExpBy3Unsafe(),
})
@send
external unsafeReplaceBy3: (t, Js_re.t, (t, t, t, t, int, t) => t) => t = "replace"

/**
`search(str, regexp)` returns the starting position of the first match of
`regexp` in the given `str`, or -1 if there is no match.

See [`String.search`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/search)
on MDN.

## Examples

```rescript
Js.String2.search("testing 1 2 3", /\d+/) == 8
Js.String2.search("no numbers", /\d+/) == -1
```
*/
@deprecated({
  reason: "Use `String.search` instead",
  migrate: String.search(),
})
@send
external search: (t, Js_re.t) => int = "search"

/**
`slice(str, from:n1, to_:n2)` returns the substring of `str` starting at
character `n1` up to but not including `n2`.
- If either `n1` or `n2` is negative, then it is evaluated as `length(str - n1)` or `length(str - n2)`.
- If `n2` is greater than the length of `str`, then it is treated as `length(str)`.
- If `n1` is greater than `n2`, slice returns the empty string.

See [`String.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice) on MDN.

## Examples

```rescript
Js.String2.slice("abcdefg", ~from=2, ~to_=5) == "cde"
Js.String2.slice("abcdefg", ~from=2, ~to_=9) == "cdefg"
Js.String2.slice("abcdefg", ~from=-4, ~to_=-2) == "de"
Js.String2.slice("abcdefg", ~from=5, ~to_=1) == ""
```
*/
@deprecated({
  reason: "Use `String.slice` instead",
  migrate: String.slice(
    ~start=%insert.labelledArgument("from"),
    ~end=%insert.labelledArgument("to_"),
  ),
})
@send
external slice: (t, ~from: int, ~to_: int) => t = "slice"

/**
`sliceToEnd(str, from:n)` returns the substring of `str` starting at character
`n` to the end of the string.
- If `n` is negative, then it is evaluated as `length(str - n)`.
- If `n` is greater than the length of `str`, then sliceToEnd returns the empty string.

See [`String.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice) on MDN.

## Examples

```rescript
Js.String2.sliceToEnd("abcdefg", ~from=4) == "efg"
Js.String2.sliceToEnd("abcdefg", ~from=-2) == "fg"
Js.String2.sliceToEnd("abcdefg", ~from=7) == ""
```
*/
@deprecated({
  reason: "Use `String.slice` instead",
  migrate: String.slice(~start=%insert.labelledArgument("from")),
})
@send
external sliceToEnd: (t, ~from: int) => t = "slice"

/**
`split(str, delimiter)` splits the given `str` at every occurrence of
`delimiter` and returns an array of the resulting substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String2.split("2018-01-02", "-") == ["2018", "01", "02"]
Js.String2.split("a,b,,c", ",") == ["a", "b", "", "c"]
Js.String2.split("good::bad as great::awful", "::") == ["good", "bad as great", "awful"]
Js.String2.split("has-no-delimiter", ";") == ["has-no-delimiter"]
```
*/
@deprecated({
  reason: "Use `String.split` instead",
  migrate: String.split(),
})
@send
external split: (t, t) => array<t> = "split"

/**
`splitAtMost delimiter ~limit: n str` splits the given `str` at every occurrence of `delimiter` and returns an array of the first `n` resulting substrings. If `n` is negative or greater than the number of substrings, the array will contain all the substrings.

```
splitAtMost "ant/bee/cat/dog/elk" "/" ~limit: 3 = [|"ant"; "bee"; "cat"|];;
splitAtMost "ant/bee/cat/dog/elk" "/" ~limit: 0 = [| |];;
splitAtMost "ant/bee/cat/dog/elk" "/" ~limit: 9 = [|"ant"; "bee"; "cat"; "dog"; "elk"|];;
```
*/
@deprecated({
  reason: "Use `String.splitAtMost` instead",
  migrate: String.splitAtMost(),
})
@send
external splitAtMost: (t, t, ~limit: int) => array<t> = "split"

/**
`splitByRe(str, regex)` splits the given `str` at every occurrence of `regex`
and returns an array of the resulting substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String2.splitByRe("art; bed , cog ;dad", /\s*[,;]\s*TODO/) == [
    Some("art"),
    Some("bed"),
    Some("cog"),
    Some("dad"),
  ]
```
*/
@deprecated({
  reason: "Use `String.splitByRegExp` instead",
  migrate: String.splitByRegExp(),
})
@send
external splitByRe: (t, Js_re.t) => array<option<t>> = "split"

/**
`splitByReAtMost(str, regex, ~limit:n)` splits the given `str` at every
occurrence of `regex` and returns an array of the first `n` resulting
substrings. If `n` is negative or greater than the number of substrings, the
array will contain all the substrings.

See [`String.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
on MDN.

## Examples

```rescript
Js.String2.splitByReAtMost("one: two: three: four", /\s*:\s*TODO/, ~limit=3) == [
    Some("one"),
    Some("two"),
    Some("three"),
  ]

Js.String2.splitByReAtMost("one: two: three: four", /\s*:\s*TODO/, ~limit=0) == []

Js.String2.splitByReAtMost("one: two: three: four", /\s*:\s*TODO/, ~limit=8) == [
    Some("one"),
    Some("two"),
    Some("three"),
    Some("four"),
  ]
```
*/
@deprecated({
  reason: "Use `String.splitByRegExpAtMost` instead",
  migrate: String.splitByRegExpAtMost(),
})
@send
external splitByReAtMost: (t, Js_re.t, ~limit: int) => array<option<t>> = "split"

/**
ES2015: `startsWith(str, substr)` returns `true` if the `str` starts with
`substr`, `false` otherwise.

See [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith)
on MDN.

## Examples

```rescript
Js.String2.startsWith("ReScript", "Re") == true
Js.String2.startsWith("ReScript", "") == true
Js.String2.startsWith("JavaScript", "Re") == false
```
*/
@deprecated({
  reason: "Use `String.startsWith` instead",
  migrate: String.startsWith(),
})
@send
external startsWith: (t, t) => bool = "startsWith"

/**
ES2015: `startsWithFrom(str, substr, n)` returns `true` if the `str` starts
with `substr` starting at position `n`, false otherwise. If `n` is negative,
the search starts at the beginning of `str`.

See [`String.startsWith`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith)
on MDN.

## Examples

```rescript
Js.String2.startsWithFrom("ReScript", "Scri", 2) == true
Js.String2.startsWithFrom("ReScript", "", 2) == true
Js.String2.startsWithFrom("JavaScript", "Scri", 2) == false
```
*/
@deprecated({
  reason: "Use `String.startsWithFrom` instead",
  migrate: String.startsWithFrom(),
})
@send
external startsWithFrom: (t, t, int) => bool = "startsWith"

/**
`substr(str, ~from:n)` returns the substring of `str` from position `n` to the
end of the string.
- If `n` is less than zero, the starting position is the length of `str - n`.
- If `n` is greater than or equal to the length of `str`, returns the empty string.

JavaScript’s `String.substr()` is a legacy function. When possible, use
`substring()` instead.

See [`String.substr`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
on MDN.

## Examples

```rescript
Js.String2.substr("abcdefghij", ~from=3) == "defghij"
Js.String2.substr("abcdefghij", ~from=-3) == "hij"
Js.String2.substr("abcdefghij", ~from=12) == ""
```
*/
@deprecated("Use `String.substring` instead") @send
external substr: (t, ~from: int) => t = "substr"

/**
`substrAtMost(str, ~from: pos, ~length: n)` returns the substring of `str` of
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
Js.String2.substrAtMost("abcdefghij", ~from=3, ~length=4) == "defg"
Js.String2.substrAtMost("abcdefghij", ~from=-3, ~length=4) == "hij"
Js.String2.substrAtMost("abcdefghij", ~from=12, ~length=2) == ""
```
*/
@deprecated("Use `String.substringAtMost` instead") @send
external substrAtMost: (t, ~from: int, ~length: int) => t = "substr"

/**
`substring(str, ~from: start, ~to_: finish)` returns characters `start` up to
but not including finish from `str`.
- If `start` is less than zero, it is treated as zero.
- If `finish` is zero or negative, the empty string is returned.
- If `start` is greater than `finish`, the `start` and `finish` points are swapped.

See [`String.substring`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring) on MDN.

## Examples

```rescript
Js.String2.substring("playground", ~from=3, ~to_=6) == "ygr"
Js.String2.substring("playground", ~from=6, ~to_=3) == "ygr"
Js.String2.substring("playground", ~from=4, ~to_=12) == "ground"
```
*/
@deprecated({
  reason: "Use `String.substring` instead",
  migrate: String.substring(
    ~start=%insert.labelledArgument("from"),
    ~end=%insert.labelledArgument("to_"),
  ),
})
@send
external substring: (t, ~from: int, ~to_: int) => t = "substring"

/**
`substringToEnd(str, ~from: start)` returns the substring of `str` from
position `start` to the end.
- If `start` is less than or equal to zero, the entire string is returned.
- If `start` is greater than or equal to the length of `str`, the empty string is returned.

See [`String.substring`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring) on MDN.

## Examples

```rescript
Js.String2.substringToEnd("playground", ~from=4) == "ground"
Js.String2.substringToEnd("playground", ~from=-3) == "playground"
Js.String2.substringToEnd("playground", ~from=12) == ""
```
*/
@deprecated({
  reason: "Use `String.substringToEnd` instead",
  migrate: String.substringToEnd(~start=%insert.labelledArgument("from")),
})
@send
external substringToEnd: (t, ~from: int) => t = "substring"

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
Js.String2.toLowerCase("ABC") == "abc"
Js.String2.toLowerCase(`ΣΠ`) == `σπ`
Js.String2.toLowerCase(`ΠΣ`) == `πς`
```
*/
@deprecated({
  reason: "Use `String.toLowerCase` instead",
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
  reason: "Use `String.toLocaleLowerCase` instead",
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
Js.String2.toUpperCase("abc") == "ABC"
Js.String2.toUpperCase(`Straße`) == `STRASSE`
Js.String2.toUpperCase(`πς`) == `ΠΣ`
```
*/
@deprecated({
  reason: "Use `String.toUpperCase` instead",
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
  reason: "Use `String.toLocaleUpperCase` instead",
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
Js.String2.trim("   abc def   ") == "abc def"
Js.String2.trim("\n\r\t abc def \n\n\t\r ") == "abc def"
```
*/
@deprecated({
  reason: "Use `String.trim` instead",
  migrate: String.trim(),
})
@send
external trim: t => t = "trim"

/* HTML wrappers */

/**
`anchor(anchorText, anchorName)` creates a string with an HTML `<a>` element
with name attribute of `anchorName` and `anchorText` as its content. Please do
not use this method, as it has been removed from the relevant web standards.

See [`String.anchor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/anchor)
on MDN.

## Examples

```rescript
Js.String2.anchor("Page One", "page1") == "<a name=\"page1\">Page One</a>"
```
*/
@deprecated("This function has been removed from the relevant web standards.") @send
external anchor: (t, t) => t = "anchor"

/**
ES2015: `link(linkText, urlText)` creates a string with an HTML `<a>` element
with href attribute of `urlText` and `linkText` as its content. Please do not
use this method, as it has been removed from the relevant web standards. See
[`String.link`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/link)
on MDN.

## Examples

```rescript
Js.String2.link("Go to page two", "page2.html") == "<a href=\"page2.html\">Go to page two</a>"
```
*/
@deprecated("This function has been removed from the relevant web standards.") @send
external link: (t, t) => t = "link"

/* FIXME: we should not encourage people to use [%identity], better
    to provide something using [@@val] so that we can track such
    casting
*/
/**
Casts its argument to an `array_like` entity that can be processed by functions
such as `Js.Array2.fromMap()`

## Examples

```rescript
let s = "abcde"
let arr = Js.Array2.fromMap(Js.String2.castToArrayLike(s), x => x)
arr == ["a", "b", "c", "d", "e"]
```
*/
@deprecated({
  reason: "Use `Array.fromString` instead",
  migrate: Array.fromString(),
})
external castToArrayLike: t => Js_array2.array_like<t> = "%identity"
