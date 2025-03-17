/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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
Contains functions available in the global scope (`window` in a browser context)
*/

/** Identify an interval started by `Js.Global.setInterval`. */
type intervalId = Stdlib_Global.intervalId

/** Identify timeout started by `Js.Global.setTimeout`. */
type timeoutId = Stdlib_Global.timeoutId

@val
/**
Clear an interval started by `Js.Global.setInterval`

## Examples

```rescript
/* API for a somewhat aggressive snoozing alarm clock */

let punchSleepyGuy = () => Js.log("Punch")

let interval = ref(Js.Nullable.null)

let remind = () => {
  Js.log("Wake Up!")
  punchSleepyGuy()
}

let snooze = mins =>
  interval := Js.Nullable.return(Js.Global.setInterval(remind, mins * 60 * 1000))

let cancel = () =>
  Js.Nullable.iter(interval.contents, (. intervalId) => Js.Global.clearInterval(intervalId))
```
*/
external clearInterval: intervalId => unit = "clearInterval"

@val
/**
Clear a timeout started by `Js.Global.setTimeout`.

## Examples

```rescript
/* A simple model of a code monkey's brain */

let closeHackerNewsTab = () => Js.log("close")

let timer = ref(Js.Nullable.null)

let work = () => closeHackerNewsTab()

let procrastinate = mins => {
  Js.Nullable.iter(timer.contents, (. timer) => Js.Global.clearTimeout(timer))
  timer := Js.Nullable.return(Js.Global.setTimeout(work, mins * 60 * 1000))
}
```
*/
external clearTimeout: timeoutId => unit = "clearTimeout"

@val
/**
Repeatedly executes a callback with a specified interval (in milliseconds)
between calls. Returns a `Js.Global.intervalId` that can be passed to
`Js.Global.clearInterval` to cancel the timeout.

## Examples

```rescript
/* Will count up and print the count to the console every second */

let count = ref(0)

let tick = () => {
  count := count.contents + 1
  Js.log(Belt.Int.toString(count.contents))
}

Js.Global.setInterval(tick, 1000)
```
*/
external setInterval: (unit => unit, int) => intervalId = "setInterval"

@val
/**
Repeatedly executes a callback with a specified interval (in milliseconds)
between calls. Returns a `Js.Global.intervalId` that can be passed to
`Js.Global.clearInterval` to cancel the timeout.

## Examples

```rescript
/* Will count up and print the count to the console every second */

let count = ref(0)

let tick = () => {
  count := count.contents + 1
  Js.log(Belt.Int.toString(count.contents))
}

Js.Global.setIntervalFloat(tick, 1000.0)
```
*/
external setIntervalFloat: (unit => unit, float) => intervalId = "setInterval"

@val
/**
Execute a callback after a specified delay (in milliseconds). Returns a
`Js.Global.timeoutId` that can be passed to `Js.Global.clearTimeout` to cancel
the timeout.

## Examples

```rescript
/* Prints "Timed out!" in the console after one second */

let message = "Timed out!"

Js.Global.setTimeout(() => Js.log(message), 1000)
```
*/
external setTimeout: (unit => unit, int) => timeoutId = "setTimeout"

@val
/**
Execute a callback after a specified delay (in milliseconds). Returns a
`Js.Global.timeoutId` that can be passed to `Js.Global.clearTimeout` to cancel
the timeout.

## Examples

```rescript
/* Prints "Timed out!" in the console after one second */

let message = "Timed out!"

Js.Global.setTimeoutFloat(() => Js.log(message), 1000.0)
```
*/
external setTimeoutFloat: (unit => unit, float) => timeoutId = "setTimeout"

@val
/**
URL-encodes a string.

See [`encodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI) on MDN.
*/
external encodeURI: string => string = "encodeURI"

@val
/**
Decodes a URL-enmcoded string produced by `encodeURI`

See [`decodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI) on MDN.
*/
external decodeURI: string => string = "decodeURI"

@val
/**
URL-encodes a string, including characters with special meaning in a URI.

See [`encodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent) on MDN.
*/
external encodeURIComponent: string => string = "encodeURIComponent"

@val
/**
Decodes a URL-enmcoded string produced by `encodeURIComponent`

See [`decodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent) on MDN.
*/
external decodeURIComponent: string => string = "decodeURIComponent"
