/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
Deprecation note: These bindings are pretty outdated and cannot be used properly
with the `->` operator.

More details on proper Promise usage can be found here:
https://rescript-lang.org/docs/manual/latest/promise#promise-legacy
*/

@@warning("-103")

type t<+'a> = promise<'a>

type error = Js_promise2.error

/*
## Examples

```rescript
type error
```
*/

@new
external make: ((~resolve: 'a => unit, ~reject: exn => unit) => unit) => promise<'a> = "Promise"

/* `make (fun resolve reject -> .. )` */
@val @scope("Promise")
external resolve: 'a => promise<'a> = "resolve"
@val @scope("Promise")
external reject: exn => promise<'a> = "reject"

@val @scope("Promise")
external all: array<promise<'a>> => promise<array<'a>> = "all"

@val @scope("Promise")
external all2: ((promise<'a0>, promise<'a1>)) => promise<('a0, 'a1)> = "all"

@val @scope("Promise")
external all3: ((promise<'a0>, promise<'a1>, promise<'a2>)) => promise<('a0, 'a1, 'a2)> = "all"

@val @scope("Promise")
external all4: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
)> = "all"

@val @scope("Promise")
external all5: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
)> = "all"

@val @scope("Promise")
external all6: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>, promise<'a5>)
) => promise<('a0, 'a1, 'a2, 'a3, 'a4, 'a5)> = "all"

@val @scope("Promise")
external race: array<promise<'a>> => promise<'a> = "race"

@send
external then_: (promise<'a>, 'a => promise<'b>) => promise<'b> = "then"
let then_ = (arg1, obj) => then_(obj, arg1)

@send
external catch: (promise<'a>, error => promise<'a>) => promise<'a> = "catch"
let catch = (arg1, obj) => catch(obj, arg1)
/* ` p|> catch handler`
    Note in JS the returned promise type is actually runtime dependent,
    if promise is rejected, it will pick the `handler` otherwise the original promise,
    to make it strict we enforce reject handler
    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch
*/

/*
let errorAsExn (x :  error) (e  : (exn ->'a option))=
  if Caml_exceptions.isCamlExceptionOrOpenVariant (Obj.magic x ) then
     e (Obj.magic x)
  else None
[%bs.error?  ]
*/
