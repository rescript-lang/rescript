// The +'a marks the abstract type parameter as covariant, which essentially means that
// a value of type 'a is immutable and may not be used in some mutable context.
//
// This makes sense for promises, since according to their specification, once a promise has
// been resolved (with a specific value), it will never change its resolved value.
//
// More details about polymorphism / invariance / covariance,... can be found here:
// https://caml.inria.fr/pub/docs/manual-ocaml/polymorphism.html#ss:variance:abstract-data-types

/***
Functions for interacting with JavaScript Promise.
See: [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).
*/

type t<+'a> = promise<'a>

/**
`resolve(value)` creates a resolved Promise with a given `value`.
See [`Promise.resolve`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) on MDN.

## Examples

```rescript
let p = Promise.resolve(5) // promise<int>
```
*/
@val
@scope("Promise")
external resolve: 'a => t<'a> = "resolve"

/**
`reject(exn)` reject a Promise.
See [`Promise.reject`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/reject) on MDN.

## Examples

```rescript
exception TestError(string)

TestError("some rejected value")
->Promise.reject
->Promise.catch(v => {
  switch v {
  | TestError(msg) => assertEqual(msg, "some rejected value")
  | _ => assert(false)
  }
  Promise.resolve()
})
->ignore
```
*/
@scope("Promise")
@val
external reject: exn => t<_> = "reject"

/**
`make(callback)` creates a new Promise based on a `callback` that receives two
uncurried functions `resolve` and `reject` for defining the Promise's result.

## Examples

```rescript
open Promise

let n = 4
Promise.make((resolve, reject) => {
  if(n < 5) {
    resolve(. "success")
  }
  else {
    reject(. "failed")
  }
})
->then(str => {
  Console.log(str)->resolve
})
->catch(_ => {
  Console.log("Error occurred")
  resolve()
})
->ignore
```
*/
@new
external make: (('a => unit, 'e => unit) => unit) => t<'a> = "Promise"

type promiseAndResolvers<'a> = {
  promise: t<'a>,
  resolve: 'a => unit,
  reject: exn => unit,
}

/**
`withResolvers()` returns a object containing a new promise with functions to resolve or reject it. See [`Promise.withResolvers`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/withResolvers) on MDN.

## Examples

```rescript
open Promise

let {promise, resolve, _} = Promise.withResolvers()

setTimeout(() => {
  resolve(. "success")
}, 1000)->TimeoutId.ignore

promise
->thenResolve(str => {
  Console.log(str)
})
->ignore
```
*/
@scope("Promise")
@val
external withResolvers: unit => promiseAndResolvers<_> = "withResolvers"

/**
`catch(promise, errorCallback)` registers an exception handler in a promise chain.
The `errorCallback` receives an `exn` value that can later be refined into a JS
error or ReScript error. The `errorCallback` needs to return a promise with the
same type as the consumed promise. See [`Promise.catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) on MDN.

## Examples

```rescript
open Promise

exception SomeError(string)

reject(SomeError("this is an error"))
->then(_ => {
  Ok("This result will never be returned")->resolve
})
->catch(e => {
  let msg = switch(e) {
    | SomeError(msg) => "ReScript error occurred: " ++ msg
    | Exn.Error(obj) =>
      switch Exn.message(obj) {
        | Some(msg) => "JS exception occurred: " ++ msg
        | None => "Some other JS value has been thrown"
      }
    | _ => "Unexpected error occurred"
  }

  Error(msg)->resolve
})
->then(result => {
  switch result {
  | Ok(r) => Console.log2("Operation successful: ", r)
  | Error(msg) => Console.log2("Operation failed: ", msg)
  }->resolve
})
->ignore // Ignore needed for side-effects
```

In case you want to return another promise in your `callback`, consider using
`then` instead.
*/
let catch: (t<'a>, exn => t<'a>) => t<'a>

/**
`then(promise, callback)` returns a new promise based on the result of `promise`'s 
value. The `callback` needs to explicitly return a new promise via `resolve`.
It is **not allowed** to resolve a nested promise (like `resolve(resolve(1))`).
See [`Promise.then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) on MDN.
## Examples

```rescript
open Promise
resolve(5)
->then(num => {
  resolve(num + 5)
})
->then(num => {
  Console.log2("Your lucky number is: ", num)
  resolve()
})
->ignore
```
*/
@send
external then: (t<'a>, 'a => t<'b>) => t<'b> = "then"

/**
`thenResolve(promise, callback)` converts an encapsulated value of a promise
into another promise wrapped value. It is **not allowed** to return a promise
within the provided callback (e.g. `thenResolve(value => resolve(value))`).

## Examples

```rescript
open Promise
resolve("Anna")
->thenResolve(str => {
  "Hello " ++ str
})
->thenResolve(str => {
  Console.log(str)
})
->ignore // Ignore needed for side-effects
```

In case you want to return another promise in your `callback`, consider using
`then` instead.
*/
@send
external thenResolve: (t<'a>, 'a => 'b) => t<'b> = "then"

/**
`finally(promise, callback)` is used to execute a function that is called no
matter if a promise was resolved or rejected. It will return the same `promise`
it originally received. See [`Promise.finally`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/finally) on MDN.

## Examples

```rescript
open Promise
exception SomeError(string)
let isDone = ref(false)

resolve(5)
->then(_ => {
  reject(SomeError("test"))
})
->then(v => {
  Console.log2("final result", v)
  resolve()
})
->catch(_ => {
  Console.log("Error handled")
  resolve()
})
->finally(() => {
  Console.log("finally")
  isDone := true
})
->then(() => {
  Console.log2("isDone:", isDone.contents)
  resolve()
})
->ignore
```
*/
@send
external finally: (t<'a>, unit => unit) => t<'a> = "finally"

/**
`race(arr)` runs all promises concurrently and returns promise settles with the eventual state of the first promise that settles. See [`Promise.race`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/race) on MDN.

## Examples

```rescript
open Promise
let racer = (ms, name) => {
  Promise.make((resolve, _) => {
    setTimeout(() => {
      resolve(name)
    }, ms)->TimeoutId.ignore
  })
}

let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

race(promises)->then(winner => {
  Console.log("The winner is " ++ winner)
  resolve()
})
```
*/
@scope("Promise")
@val
external race: array<t<'a>> => t<'a> = "race"

/**
`any(arr)` runs all promises concurrently and returns promise fulfills when any of the input's promises fulfills, with this first fulfillment value. See [`Promise.any`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/any) on MDN.

## Examples

```rescript
open Promise
let racer = (ms, name) => {
  Promise.make((resolve, _) => {
    setTimeout(() => {
      resolve(name)
    }, ms)->TimeoutId.ignore
  })
}

let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

any(promises)->then(winner => {
  Console.log("The winner is " ++ winner)
  resolve()
})
```
*/
@scope("Promise")
@val
external any: array<t<'a>> => t<'a> = "any"

/**
`all(promises)` runs all promises concurrently and returns a promise fulfills when all of the input's promises fulfill, with an array of the fulfillment values. See [`Promise.all`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all) on MDN.

```rescript
open Promise
let promises = [resolve(1), resolve(2), resolve(3)]

all(promises)
->then((results) => {
  results->Array.forEach(num => {
    Console.log2("Number: ", num)
  })

  resolve()
})
->ignore
```
*/
@scope("Promise")
@val
external all: array<t<'a>> => t<array<'a>> = "all"

/**
`all2((p1, p2))`. Like `all()`, but with a fixed size tuple of 2
*/
@scope("Promise")
@val
external all2: ((t<'a>, t<'b>)) => t<('a, 'b)> = "all"

/**
`all3((p1, p2, p3))`. Like `all()`, but with a fixed size tuple of 3
*/
@scope("Promise")
@val
external all3: ((t<'a>, t<'b>, t<'c>)) => t<('a, 'b, 'c)> = "all"

/**
`all4((p1, p2, p3, p4))`. Like `all()`, but with a fixed size tuple of 4
*/
@scope("Promise")
@val
external all4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<('a, 'b, 'c, 'd)> = "all"

/**
`all5((p1, p2, p3, p4, p5))`. Like `all()`, but with a fixed size tuple of 5
*/
@scope("Promise")
@val
external all5: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>)) => t<('a, 'b, 'c, 'd, 'e)> = "all"

/**
`all6((p1, p2, p4, p5, p6))`. Like `all()`, but with a fixed size tuple of 6
")*/
@scope("Promise")
@val
external all6: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>)) => t<('a, 'b, 'c, 'd, 'e, 'f)> = "all"

@tag("status")
type settledResult<+'a> =
  | @as("fulfilled") Fulfilled({value: 'a}) | @as("rejected") Rejected({reason: exn})

/**
`allSettled(promises)` runs all promises concurrently and returns promise fulfills when all of the input's promises settle with an array of objects that describe the outcome of each promise. See [`Promise.allSettled`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/allSettled) on MDN.

```rescript
open Promise

exception TestError(string)

let promises = [resolve(1), resolve(2), reject(TestError("some rejected promise"))]

allSettled(promises)
->then((results) => {
  results->Array.forEach((result) => {
    switch result {
      | Fulfilled({value: num}) => 
        Console.log2("Number: ", num)
      | Rejected({reason}) =>
        Console.log(reason)
    }
  })

  resolve()
})
->ignore
```
*/
@scope("Promise")
@val
external allSettled: array<t<'a>> => t<array<settledResult<'a>>> = "allSettled"

/**
`allSettled2((p1, p2))`. Like `allSettled()`, but with a fixed size tuple of 2
*/
@scope("Promise")
@val
external allSettled2: ((t<'a>, t<'b>)) => t<(settledResult<'a>, settledResult<'b>)> = "allSettled"

/**
`allSettled3((p1, p2, p3))`. Like `allSettled()`, but with a fixed size tuple of 3
*/
@scope("Promise")
@val
external allSettled3: ((t<'a>, t<'b>, t<'c>)) => t<(
  settledResult<'a>,
  settledResult<'b>,
  settledResult<'c>,
)> = "allSettled"

/**
`allSettled4((p1, p2, p3, p4))`. Like `allSettled()`, but with a fixed size tuple of 4
*/
@scope("Promise")
@val
external allSettled4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<(
  settledResult<'a>,
  settledResult<'b>,
  settledResult<'c>,
  settledResult<'d>,
)> = "allSettled"

/**
`allSettled5((p1, p2, p3, p4, p5))`. Like `allSettled()`, but with a fixed size tuple of 5
*/
@scope("Promise")
@val
external allSettled5: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>)) => t<(
  settledResult<'a>,
  settledResult<'b>,
  settledResult<'c>,
  settledResult<'d>,
  settledResult<'e>,
)> = "allSettled"

/**
`allSettled6((p1, p2, p4, p5, p6))`. Like `allSettled()`, but with a fixed size tuple of 6
")*/
@scope("Promise")
@val
external allSettled6: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>)) => t<(
  settledResult<'a>,
  settledResult<'b>,
  settledResult<'c>,
  settledResult<'d>,
  settledResult<'e>,
  settledResult<'f>,
)> = "allSettled"

/**
`done(p)` is a safe way to ignore a promise. If a value is anything else than a
promise, it will raise a type error.
*/
@deprecated("Please use `Promise.ignore` instead")
external done: promise<'a> => unit = "%ignore"

/**
  `ignore(promise)` ignores the provided promise and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: promise<'a> => unit = "%ignore"
