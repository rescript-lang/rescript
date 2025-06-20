type vehicle = {name: string}

@react.component
let make = (~vehicle) => {
  let (count, setCount) = React.useState(() => 0)

  <div>
    <p>
      {React.string(
        "Hooks example " ++ (vehicle.name ++ (" clicked " ++ (Int.toString(count) ++ " times"))),
      )}
    </p>
    <button onClick={_ => setCount(_ => count + 1)}> {React.string("Click me")} </button>
    <ImportHooks person={name: "Mary", age: 71} renderMe={x => React.string(x["randomString"])}>
      {React.string("child1")} {React.string("child2")}
    </ImportHooks>
    <ImportHookDefault
      person={name: "DefaultImport", age: 42} renderMe={x => React.string(x["randomString"])}>
      {React.string("child1")} {React.string("child2")}
    </ImportHookDefault>
  </div>
}

@genType
let default = make

module Inner = {
  @genType @react.component
  let make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>

  module Inner2 = {
    @genType @react.component
    let make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>
  }
}

module NoProps = {
  @genType @react.component
  let make = () => <div> React.null </div>
}

type cb = (~_to: vehicle) => unit

@genType
let functionWithRenamedArgs = (~_to, ~_Type, ~cb: cb) => {
  cb(~_to)
  _to.name ++ _Type.name
}

type r = {x: string}

@genType
type testReactContext = React.Context.t<int>

@genType
type testReactRef = React.ref<int>

@genType
type testDomRef = ReactDOM.domRef

module RenderPropRequiresConversion = {
  @genType @react.component
  let make = (~renderVehicle: {"vehicle": vehicle, "number": int} => React.element) => {
    let car = {name: "Car"}
    renderVehicle({"vehicle": car, "number": 42})
  }
}
