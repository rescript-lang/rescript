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

let let_ = (p, cb) => Promise.then(cb, p)

let mapAsync = (p, cb) => Promise.then(a => cb(a)->Promise.resolve, p)

let async = a => Promise.resolve(a)

type promise<'a> = Promise.t<'a>

let catchAsync = (p, cb) => Promise.catch(p, cb)

let asyncFromResult = result =>
  // Lift it into a promise in case the original caller wasn't already in the promise. We want to use Promise's error catching behavior, and not Javascript's error catching behavior.
  result
  ->async
  ->mapAsync(a =>
    switch a {
    | Ok(b) => b
    | Error(err) => Exn.raiseError(err->Obj.magic)
    }
  )

let attemptMapAsync = (
  promise: Promise.t<'a>,
  attempter: 'a => result<'b, 'error>,
): Promise.t<'b> =>
  promise->mapAsync(a =>
    switch attempter(a) {
    | Ok(b) => b
    | Error(err) => Exn.raiseError(err->Obj.magic)
    }
  )
