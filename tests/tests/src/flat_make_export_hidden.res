module A = {
  module B = {
    let make = () => 1
  }
}

// Hidden by the interface, so it does not clash with the flat export.
let \"A$B$make" = () => Math.random() > 2. ? 0 : 2

let both = () => A.B.make() + \"A$B$make"()
