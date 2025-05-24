module X = {
    type t
}

module Y = {
    open X

    let z = (x: t) => ""

    let a = (x:t) => {
        // x.
        //   ^com
        ()
    }

    let b = (x:t) => 4
}


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
    }
)
 => {
    open Types

    @send
    external addSprite: (context, string) => comp = "sprite"
 }

external k: Types.context = "k"

module Wall = {
    type t

    include PosComp({ type t = t })

    let blah = (k: Types.context) => ""

    let make = () => {
        [
            // k.
            //   ^com

            // add
            //    ^com
        ]
    }

    module Poster = {
        type t

        include SpriteComp({ type t = t })

        let make = () => {
            [
                // k.
                //   ^com
            ]
        }
    }
}