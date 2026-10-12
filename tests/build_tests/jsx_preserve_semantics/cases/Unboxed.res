// An unboxed props record is the value of its field, not an object

%%raw(`
function lowerUnboxed(props) { return null }
`)

@unboxed type props = {title: string}

module Comp = {
  @react.componentWithProps
  let make = (props: props) => <div title=props.title />
}
module Lower = {
  @val external make: React.component<props> = "lowerUnboxed"
}

let getProps = (): props => {title: "fn"}

let literal = <Comp title="x" />
let literalKeyed = <Comp title="x" key="k" />
let spread = <Comp {...getProps()} />
let lowerLiteral = <Lower title="y" />
let lowerSpread = <Lower {...getProps()} />
