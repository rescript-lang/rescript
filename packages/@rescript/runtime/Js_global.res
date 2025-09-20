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
@deprecated({
  reason: "Use `intervalId` directly instead.",
  migrate: %replace.type(: intervalId),
})
type intervalId = Stdlib_Global.intervalId

/** Identify timeout started by `Js.Global.setTimeout`. */
@deprecated({
  reason: "Use `timeoutId` directly instead.",
  migrate: %replace.type(: timeoutId),
})
type timeoutId = Stdlib_Global.timeoutId

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

let snooze = mins => interval := Js.Nullable.return(Js.Global.setInterval(remind, mins * 60 * 1000))

let cancel = () =>
  Js.Nullable.iter(interval.contents, intervalId => Js.Global.clearInterval(intervalId))
```
*/
@deprecated({
  reason: "Use `clearInterval` instead.",
  migrate: clearInterval(),
})
@val
external clearInterval: intervalId => unit = "clearInterval"

/**
Clear a timeout started by `Js.Global.setTimeout`.

## Examples

```rescript
/* A simple model of a code monkey's brain */

let closeHackerNewsTab = () => Js.log("close")

let timer = ref(Js.Nullable.null)

let work = () => closeHackerNewsTab()

let procrastinate = mins => {
  Js.Nullable.iter(timer.contents, timer => Js.Global.clearTimeout(timer))
  timer := Js.Nullable.return(Js.Global.setTimeout(work, mins * 60 * 1000))
}
```
*/
@deprecated({
  reason: "Use `clearTimeout` instead.",
  migrate: clearTimeout(),
})
@val
external clearTimeout: timeoutId => unit = "clearTimeout"

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
@deprecated({
  reason: "Use `setInterval` instead.",
  migrate: setInterval(),
})
@val
external setInterval: (unit => unit, int) => intervalId = "setInterval"

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
@deprecated({
  reason: "Use `setIntervalFloat` instead.",
  migrate: setIntervalFloat(),
})
@val
external setIntervalFloat: (unit => unit, float) => intervalId = "setInterval"

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
@deprecated({
  reason: "Use `setTimeout` instead.",
  migrate: setTimeout(),
})
@val
external setTimeout: (unit => unit, int) => timeoutId = "setTimeout"

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
@deprecated({
  reason: "Use `setTimeoutFloat` instead.",
  migrate: setTimeoutFloat(),
})
@val
external setTimeoutFloat: (unit => unit, float) => timeoutId = "setTimeout"

/**
URL-encodes a string.

See [`encodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI) on MDN.
*/
@deprecated({
  reason: "Use `encodeURI` instead.",
  migrate: encodeURI(),
})
@val
external encodeURI: string => string = "encodeURI"

/**
Decodes a URL-enmcoded string produced by `encodeURI`

See [`decodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI) on MDN.
*/
@deprecated({
  reason: "Use `decodeURI` instead.",
  migrate: decodeURI(),
})
@val
external decodeURI: string => string = "decodeURI"

/**
URL-encodes a string, including characters with special meaning in a URI.

See [`encodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent) on MDN.
*/
@deprecated({
  reason: "Use `encodeURIComponent` instead.",
  migrate: encodeURIComponent(),
})
@val
external encodeURIComponent: string => string = "encodeURIComponent"

/**
Decodes a URL-enmcoded string produced by `encodeURIComponent`

See [`decodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent) on MDN.
*/
@deprecated({
  reason: "Use `decodeURIComponent` instead.",
  migrate: decodeURIComponent(),
})
@val
external decodeURIComponent: string => string = "decodeURIComponent"
