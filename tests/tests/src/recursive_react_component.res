@@config({
  flags: [
    "-bs-jsx",
    "4", // "-dsource",
    // "-w","A",
    // "-warn-error", "a"
  ],
})

@react.component
let rec make = (~foo, ()) => React.createElement(make, {foo: foo})

module ShadowedSelfReference = {
  type childProps = {foo: int}

  module Child = {
    @react.component
    let make = (~foo) => React.int(foo)
  }

  @react.component
  let rec make = (~make: React.component<childProps>, ~foo, ()) =>
    React.createElement(make, {foo: foo})
}
