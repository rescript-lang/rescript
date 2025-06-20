module Types = {
  type comp

  type context

  type vec2
}

module PosComp = (
  T: {
    type t
  },
) => {
  open Types

  @send
  external addPos: (context, float, float) => comp = "pos"

  @send
  external addPosFromVec2: (context, vec2) => comp = "pos"
}

module SpriteComp = (
  T: {
    type t
  },
) => {
  open Types

  @send
  external addSprite: (context, string) => comp = "sprite"
}

external k: Types.context = "k"

@send
external add: (Types.context, array<Types.comp>) => 't = "add"

module Wall = {
  type t

  include PosComp({type t = t})

  let blah = (k: Types.context) => ""

  let make = () => {
    [
      // k.
      //   ^com
      // add
      //    ^com
    ]
  }

  let makeWith = x => {
    k->add([
      k->addPos(1.0, 2.0),

      // addP
      //     ^com
    ])
  }

  module Poster = {
    type t

    include SpriteComp({type t = t})

    let make = () => {
      [
        // k.
        //   ^com
      ]
    }
  }
}

module M = {
  let lex = (a: int) => "foo"
}

module N = {
  include M

  let a = 4
  // let o = a.l
  //            ^com

  // let _ = l
  //          ^com
}
