/***Module level documentation goes here. */

/** This type represents stuff. */
type t = {
  /** The name of the stuff.*/
  name: string,
  /** Whether stuff is online.*/
  online: bool,
}

/** Create stuff.

```rescript example
let stuff = make("My name")
```
*/
let make = name => {
  name,
  online: true,
}

/** Stuff goes offline.*/
let asOffline = (t: t) => {...t, online: false}

/** exotic identifier */
let \"SomeConstant" = 12

module SomeInnerModule = {
  /*** Another module level docstring here.*/
  type status =
    /** If this is started or not */
    | Started(t)
    /** Stopped? */
    | Stopped
    /** Now idle.*/
    | Idle

  /** These are all the valid inputs.*/
  type validInputs = [#something | #"needs-escaping" | #withPayload(int) | #status(status)]

  type callback = (t, ~status: status) => unit
}

module AnotherModule = {
  /*** Mighty fine module here too!*/

  /** This links another module. Neat. */
  module LinkedModule = SomeInnerModule

  /**
  Testing what this looks like.*/
  type callback = SomeInnerModule.status => unit

  let isGoodStatus = (status: SomeInnerModule.status) => status == Stopped

  /** Trying how it looks with an inline record in a variant. */
  type someVariantWithInlineRecords =
    /** This has inline records...*/
    | SomeStuff({
        offline: bool,
        /** Is the user online? */
        online?: bool,
      })

  open ReactDOM

  /**Callback to get the DOM root...*/
  type domRoot = unit => Client.Root.t
}

module ModuleWithThingsThatShouldNotBeExported: {
  /*** BROKEN: This docstring isn't picked up Doesn't seem to be parsed at all, no attributes found.*/

  /** The type t is stuff. */
  type t

  /** The maker of stuff!*/
  let make: unit => t
} = {
  /*** Mighty fine module here too!*/
  type t = string
  type x = int
  type f = bool

  let m1 = (x: x) => {
    x + 1
  }

  let m2 = (f: f) =>
    if f {
      true
    } else {
      false
    }

  let make = () => {
    if m2(true) && m1(1) > 2 {
      "1"
    } else {
      "2"
    }
  }
}

module type Example = {
  /***
  this is an example module type 
  */

  /**
  main type of this module 
  */
  type t

  /**
  function from t to t
  */
  let f: t => t
}

module M: Example = {
  /***
  implementation of Example module type
  */

  /**
  main type 
  */
  type t = int

  /**
  identity function
  */
  let f = (x: int) => x
}

module type MT = {
  let x: int
}

module A: MT = {
  let x = 42
}

module C = {
  module D: MT = {
    let x = 42
  }
}

// ^dex
