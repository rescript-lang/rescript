module Icon = {
  @react.component
  let make = () => <strong />
}

// Local module whose make is a variable not named `make`
let localComp = () => {
  let comp = React.memo(Icon.make)
  module M = {
    let make = comp
  }
  <M />
}
let localCompResult = localComp()

let localCompKeyed = () => {
  let comp = React.memo(Icon.make)
  module M = {
    let make = comp
  }
  <M key="k" />
}
let localCompKeyedResult = localCompKeyed()

let localCompChildren = () => {
  let comp = React.memo(Icon.make)
  module M = {
    let make = comp
  }
  <M></M>
}
let localCompChildrenResult = localCompChildren()

// Top-level module aliasing a lowercase top-level value
let topComp = React.memo(Icon.make)
module T = {
  let make = topComp
}
let topAlias = <T />
let topAliasKeyed = <T key="k" />
let topAliasMulti =
  <div>
    <T />
    <T />
  </div>
