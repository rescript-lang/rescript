/*** Module level doc here.*/

/** Type t is pretty cool.*/
type t

/** Makerz of stuffz. */
let make: unit => t

module InnerModule: {
  /*** This inner module is nice...*/

  /** This type is also t. */
  type t

  /** Maker of tea.*/
  let make: unit => t
}

// ^dex
