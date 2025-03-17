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
Provide bindings to JS regular expressions (RegExp).

**Note:** This is not an immutable API. A RegExp object with the `global` ("g")
flag set will modify the [`lastIndex`]() property when the RegExp object is used,
and subsequent uses will continue the search from the previous [`lastIndex`]().
*/

/** The RegExp object. */
type t = Stdlib_RegExp.t

/** The result of a executing a RegExp on a string. */
type result

/**
An `array` of the match and captures, the first is the full match and the
remaining are the substring captures.
*/
external captures: result => array<Js_null_undefined.t<string>> = "%identity"

@deprecated("Use Js.Re.captures instead")
external matches: result => array<string> = "%identity"

/** 0-based index of the match in the input string. */
@get
external index: result => int = "index"

/** The original input string. */
@get
external input: result => string = "input"

/**
Constructs a RegExp object (Js.Re.t) from a `string`.
Regex literals `/.../` should generally be preferred, but `fromString`
is useful when you need to dynamically construct a regex using strings,
exactly like when you do so in JavaScript.

## Examples

```rescript
let firstReScriptFileExtension = (filename, content) => {
  let result = Js.Re.fromString(filename ++ "\.(res|resi)")->Js.Re.exec_(content)
  switch result {
  | Some(r) => Js.Nullable.toOption(Js.Re.captures(r)[1])
  | None => None
  }
}

// outputs "res"
firstReScriptFileExtension("School", "School.res School.resi Main.js School.bs.js")
```
*/
@new
external fromString: string => t = "RegExp"

/**
Constructs a RegExp object (`Js.Re.t`) from a string with the given flags.
See `Js.Re.fromString`.

Valid flags:

- **g** global
- **i** ignore case
- **m** multiline
- **u** unicode (es2015)
- **y** sticky (es2015)
*/
@new
external fromStringWithFlags: (string, ~flags: string) => t = "RegExp"

/** Returns the enabled flags as a string. */
@get
external flags: t => string = "flags"

/** Returns a `bool` indicating whether the global flag is set. */
@get
external global: t => bool = "global"

/** Returns a `bool` indicating whether the ignoreCase flag is set. */
@get
external ignoreCase: t => bool = "ignoreCase"

/**
Returns the index where the next match will start its search. This property
will be modified when the RegExp object is used, if the global ("g") flag is
set.

## Examples

```rescript
let re = /ab*TODO/g
let str = "abbcdefabh"

let break = ref(false)
while !break.contents {
  switch Js.Re.exec_(re, str) {
  | Some(result) => Js.Nullable.iter(Js.Re.captures(result)[0], (. match_) => {
      let next = Belt.Int.toString(Js.Re.lastIndex(re))
      Js.log("Found " ++ (match_ ++ (". Next match starts at " ++ next)))
    })
  | None => break := true
  }
}
```

See
[`RegExp: lastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/lastIndex)
on MDN.
*/
@get
external lastIndex: t => int = "lastIndex"

/** Sets the index at which the next match will start its search from. */
@set
external setLastIndex: (t, int) => unit = "lastIndex"

/** Returns a `bool` indicating whether the multiline flag is set. */
@get
external multiline: t => bool = "multiline"

/** Returns the pattern as a `string`. */
@get
external source: t => string = "source"

/** Returns a `bool` indicating whether the sticky flag is set. */
@get
external sticky: t => bool = "sticky"

/** Returns a `bool` indicating whether the unicode flag is set. */
@get
external unicode: t => bool = "unicode"

/**
Executes a search on a given string using the given RegExp object.
Returns `Some(Js.Re.result)` if a match is found, `None` otherwise.

## Examples

```rescript
/* Match "quick brown" followed by "jumps", ignoring characters in between
 * Remember "brown" and "jumps"
  * Ignore case
  */

let re = /quick\s(brown).+?(jumps)/ig
let result = Js.Re.exec_(re, "The Quick Brown Fox Jumps Over The Lazy Dog")
```

See [`RegExp.prototype.exec()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec)
on MDN.
*/
@send
@return(null_to_opt)
external exec_: (t, string) => option<result> = "exec"

/**
Tests whether the given RegExp object will match a given `string`.
Returns true if a match is found, false otherwise.

## Examples

```rescript
/* A simple implementation of Js.String.startsWith */

let str = "hello world!"

let startsWith = (target, substring) =>
  Js.Re.fromString("^" ++ substring)->Js.Re.test_(target)

Js.log(str->startsWith("hello")) /* prints "true" */
```

See [`RegExp.prototype.test()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/test)
on MDN.
*/
@send
external test_: (t, string) => bool = "test"
