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
Provide utilities for JS dictionary object.

**Note:** This module's examples will assume this predeclared dictionary:

## Examples

```rescript
let ages = Js.Dict.fromList(list{("Maria", 30), ("Vinh", 22), ("Fred", 49)})
```
*/

/*
Dictionary type (ie an '{ }' JS object). However it is restricted to hold a
single type; therefore values must have the same type. This Dictionary type is
mostly used with the Js_json.t type.
*/
@deprecated({
  reason: "Use `dict` directly instead.",
  migrate: %replace.type(: dict),
})
type t<'a> = dict<'a>

/**
  The type for dictionary keys. This means that dictionaries *must* use `string`s as their keys.
*/
@deprecated({
  reason: "Use `string` directly instead.",
  migrate: %replace.type(: string),
})
type key = string

/**
`Js.Dict.get(key)` returns `None` if the key is not found in the dictionary,
`Some(value)` otherwise.

## Examples

```rescript
Js.Dict.get(ages, "Vinh") == Some(22)
Js.Dict.get(ages, "Paul") == None
```
*/
@deprecated({
  reason: "Use `Dict.get` instead.",
  migrate: Dict.get(),
})
let get: (t<'a>, key) => option<'a>

/**
`Js.Dict.unsafeGet(key)` returns the value if the key exists, otherwise an `undefined` value is returned. Use this only when you are sure the key exists (i.e. when having used the `keys()` function to check that the key is valid).

## Examples

```rescript
Js.Dict.unsafeGet(ages, "Fred") == 49
Js.Dict.unsafeGet(ages, "Paul") // returns undefined
```
*/
@deprecated({
  reason: "Use `Dict.getUnsafe` instead.",
  migrate: Dict.getUnsafe(),
})
@get_index
external unsafeGet: (t<'a>, key) => 'a = ""

/**
`Js.Dict.set(dict, key, value)` sets the key/value in the dictionary `dict`. If
the key does not exist, and entry will be created for it.

*This function modifies the original dictionary.*

## Examples

```rescript
Js.Dict.set(ages, "Maria", 31)
Js.log(ages == Js.Dict.fromList(list{("Maria", 31), ("Vinh", 22), ("Fred", 49)}))

Js.Dict.set(ages, "David", 66)
Js.log(ages == Js.Dict.fromList(list{("Maria", 31), ("Vinh", 22), ("Fred", 49), ("David", 66)}))
```
*/
@deprecated({
  reason: "Use `Dict.set` instead.",
  migrate: Dict.set(),
})
@set_index
external set: (t<'a>, key, 'a) => unit = ""

/**
Returns all the keys in the dictionary `dict`.

## Examples

```rescript
Js.Dict.keys(ages) == ["Maria", "Vinh", "Fred"]
```
*/
@deprecated({
  reason: "Use `Dict.keysToArray` instead.",
  migrate: Dict.keysToArray(),
})
@val
external keys: t<'a> => array<string> = "Object.keys"

/** Returns an empty dictionary. */
@deprecated({
  reason: "Use `Dict.make` instead.",
  migrate: Dict.make(),
})
@obj
external empty: unit => t<'a> = ""

/** Experimental internal function */
@deprecated({
  reason: "Use `Dict.delete` instead.",
  migrate: Dict.delete(),
})
let unsafeDeleteKey: (t<string>, string) => unit

/**
Returns an array of key/value pairs in the given dictionary (ES2017).

## Examples

```rescript
Js.Dict.entries(ages) == [("Maria", 30), ("Vinh", 22), ("Fred", 49)]
```
*/
@deprecated({
  reason: "Use `Dict.toArray` instead.",
  migrate: Dict.toArray(),
})
let entries: t<'a> => array<(key, 'a)>

/**
Returns the values in the given dictionary (ES2017).

## Examples

```rescript
Js.Dict.values(ages) == [30, 22, 49]
```
*/
@deprecated({
  reason: "Use `Dict.valuesToArray` instead.",
  migrate: Dict.valuesToArray(),
})
let values: t<'a> => array<'a>

/**
Creates a new dictionary containing each (key, value) pair in its list
argument.

## Examples

```rescript
let capitals = Js.Dict.fromList(list{("Japan", "Tokyo"), ("France", "Paris"), ("Egypt", "Cairo")})
```
*/
@deprecated("Use `Dict.fromArray(List.toArray(...))` instead.")
let fromList: list<(key, 'a)> => t<'a>

/**
Creates a new dictionary containing each (key, value) pair in its array
argument.

## Examples

```rescript
let capitals2 = Js.Dict.fromArray([("Germany", "Berlin"), ("Burkina Faso", "Ouagadougou")])
```
*/
@deprecated({
  reason: "Use `Dict.fromArray` instead.",
  migrate: Dict.fromArray(),
})
let fromArray: array<(key, 'a)> => t<'a>

/**
`map(f, dict)` maps `dict` to a new dictionary with the same keys, using the
function `f` to map each value.

## Examples

```rescript
let prices = Js.Dict.fromList(list{("pen", 1.00), ("book", 5.00), ("stapler", 7.00)})

let discount = price => price *. 0.90
let salePrices = Js.Dict.map(discount, prices)

salePrices == Js.Dict.fromList(list{("pen", 0.90), ("book", 4.50), ("stapler", 6.30)})
```
*/
@deprecated({
  reason: "Use `Dict.mapValues` instead.",
  migrate: Dict.mapValues(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Dict.mapValues(),
})
let map: ('a => 'b, t<'a>) => t<'b>
