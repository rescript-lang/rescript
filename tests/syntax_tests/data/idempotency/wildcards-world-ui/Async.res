// This is coppied directly from: https://github.com/mrmurphy/serbet/blob/master/src/Async.re
// This file is designed to be opened for entire modules.

// Using Bluebird for the global promise implementation allows actually useful
// stack traces to be generated for debugging runtime issues.
%%raw(`global.Promise = require('bluebird')`)
%%raw(`
Promise.config({
  warnings: false
})
`)

let let_ = (p, cb) => Js.Promise.then_(cb, p)

let mapAsync = (p, cb) => Js.Promise.then_(a => cb(a)->Js.Promise.resolve, p)

let async = a => Js.Promise.resolve(a)

type promise<'a> = Js.Promise.t<'a>

let catchAsync = (p, cb) => Js.Promise.catch(cb, p)

let asyncFromResult = result =>
  // Lift it into a promise in case the original caller wasn't already in the promise. We want to use Promise's error catching behavior, and not Javascript's error catching behavior.
  result
  ->async
  ->mapAsync(a =>
    switch a {
    | Ok(b) => b
    | Error(err) => Js.Exn.raiseError(err->Obj.magic)
    }
  )

let attemptMapAsync = (
  promise: Js.Promise.t<'a>,
  attempter: 'a => result<'b, 'error>,
): Js.Promise.t<'b> =>
  promise->mapAsync(a =>
    switch attempter(a) {
    | Ok(b) => b
    | Error(err) => Js.Exn.raiseError(err->Obj.magic)
    }
  )
