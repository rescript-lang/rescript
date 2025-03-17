// These are bindings used in RxjsCompletion.res
// We are using a separate file to test complication for modules of external files.
type target

module Subscriber = {
  type t<'t> = {next: 't => unit}
}

module Observable = {
  // Complete items defined inside the parent module.
  @editor.completeFrom(Rxjs)
  type t<'t>

  type dispose = unit => unit

  @new @module("rxjs")
  external make: (Subscriber.t<'t> => dispose) => t<'t> = "Observable"

  type subscription

  @send
  external subscribe: (t<'t>, 't => unit) => subscription = "subscribe"
}

@module("rxjs")
external fromEvent: (target, string) => Observable.t<'t> = "fromEvent"

type operation<'t, 'u>

@send
external pipe: (Observable.t<'t>, operation<'t, 'u>) => Observable.t<'u> = "pipe"

@send
external pipe2: (Observable.t<'t>, operation<'t, 'u>, operation<'u, 'i>) => Observable.t<'i> =
  "pipe"

@module("rxjs")
external map: ('t => 'u) => operation<'t, 'u> = "map"

@module("rxjs")
external distinctUntilChanged: unit => operation<'t, 't> = "distinctUntilChanged"

@module("rxjs")
external merge: (Observable.t<'t>, Observable.t<'t>) => Observable.t<'t> = "merge"

@module("rxjs")
external scan: (('acc, 't) => 'acc, 'acc) => operation<'t, 'acc> = "scan"

@module("rxjs")
external combineLatest: (Observable.t<'a>, Observable.t<'b>) => Observable.t<('a, 'b)> =
  "combineLatest"
