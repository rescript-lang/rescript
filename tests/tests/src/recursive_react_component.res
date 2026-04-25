@@config({
  flags: [
    "-bs-jsx",
    "4", // "-dsource",
    // "-w","A",
    // "-warn-error", "a"
  ],
})

@react.component
let rec make = (~foo) => React.createElement(React.component(make), {foo: foo})

module ShadowedSelfReference = {
  type childProps = {foo: int}

  @react.component
  let rec make = (~make: React.component<childProps>, ~foo) => React.createElement(make, {foo: foo})
}

module Leaf = {
  @react.component
  let make = (~foo) => React.int(foo)
}

module ShadowedByLocalLet = {
  @react.component
  let rec make = (~foo) => {
    let make = Leaf.make
    React.createElement(make, {foo: foo})
  }
}

module ShadowedByNestedParameter = {
  @react.component
  let rec make = (~foo) => {
    let render = (make: React.component<Leaf.props<int>>) =>
      React.createElement(make, ({foo: foo}: Leaf.props<int>))
    render(Leaf.make)
  }
}
