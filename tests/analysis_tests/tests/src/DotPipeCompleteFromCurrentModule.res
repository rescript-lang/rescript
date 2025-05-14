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