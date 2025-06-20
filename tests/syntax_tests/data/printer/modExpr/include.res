type t

include Sprite.Comp({
    type t = t
})
include Color.Comp({
    type t = t
})
include Pos.Comp({
type t = superLooooooooooooooooooooooooooooooooooooooooooooooooooooooooonnnnnnnnngggggTyyyyyyypeeeNaaaaaammeee
})
include Area.Comp({
    type t = t
})
include Anchor.Comp({
    type t = t
})
include Move.Comp({
    type t = t
})
include OffScreen.Comp({
    type t = t
})
include Health.Comp({
    type t = t
})
include Opacity.Comp({
    type t = t
})

module Nested = {
    type t

    include Sprite.Comp({
        type t = t
    })
    include Color.Comp({
        type t = t
    })
    include Pos.Comp({
        type t = t
    })
    include Area.Comp({
        type t = t
    })
    include Anchor.Comp({
        type t = t
    })
    include Move.Comp({
        type t = t
    })
    include OffScreen.Comp({
        type t = t
    })
    include Health.Comp({
        type t = t
    })
    include Opacity.Comp({
        type t = t
    })
}