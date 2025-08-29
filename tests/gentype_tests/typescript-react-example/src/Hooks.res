@@warning("-3")

type vehicle = {name: string}

@react.component
let make = (~vehicle) => {
  let (count, setCount) = React.useState(() => 0)

  <div>
    <p>
      {React.string(
        "Hooks example " ++
        (vehicle.name ++
        (" clicked " ++ (Belt.Int.toString(count) ++ " times"))),
      )}
    </p>
    <button onClick={_ => setCount(_ => count + 1)}> {React.string("Click me")} </button>
    <ImportHooks person={name: "Mary", age: 71} renderMe={x => React.string(x["randomString"])}>
      {React.string("child1")}
      {React.string("child2")}
    </ImportHooks>
    <ImportHookDefault
      person={name: "DefaultImport", age: 42} renderMe={x => React.string(x["randomString"])}
    >
      {React.string("child1")}
      {React.string("child2")}
    </ImportHookDefault>
  </div>
}

@genType
let default = make

module Another = {
  @genType @react.component
  let anotherComponent = (~vehicle, ~callback: unit => unit) => {
    callback()
    <div> {React.string("Another Hook " ++ vehicle.name)} </div>
  }
}

module Inner = {
  @genType @react.component
  let make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>
  module Another = {
    @genType @react.component
    let anotherComponent = (~vehicle) =>
      <div> {React.string("Another Hook " ++ vehicle.name)} </div>
  }
  module Inner2 = {
    @genType @react.component
    let make = (~vehicle) => <div> {React.string("Another Hook " ++ vehicle.name)} </div>
    module Another = {
      @genType @react.component
      let anotherComponent = (~vehicle) =>
        <div> {React.string("Another Hook " ++ vehicle.name)} </div>
    }
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

module WithRename = {
  @genType @react.component
  let componentWithRenamedArgs = (~_to, ~_Type, ~cb: cb) => {
    cb(~_to)
    React.string(_to.name ++ _Type.name)
  }
}

module WithRef = {
  @genType @react.component
  let makeWithRef = (~vehicle) => {
    let _ = 34
    ref =>
      switch ref->Js.Nullable.toOption {
      | Some(ref) => <button ref={ReactDOM.Ref.domRef(ref)}> {React.string(vehicle.name)} </button>
      | None => React.null
      }
  }
}

@genType let testForwardRef = React.forwardRef((x, y) => WithRef.makeWithRef(x)(y))

type r = {x: string}

module ForwardRef = {
  @genType
  let input = React.forwardRef((r, ref) => <div ref={Obj.magic(ref)}> {React.string(r.x)} </div>)
}

@genType type callback<'input, 'output> = 'input => 'output

@genType type testReactContext = React.Context.t<int>

@genType type testReactRef = React.Ref.t<int>

@genType type testDomRef = ReactDOM.domRef

@genType type testDomRef2 = ReactDOM.Ref.t

module Poly = {
  @genType @react.component
  let polymorphicComponent = (~p as (x, _)) => React.string(x.name)
}

module Fun = {
  @genType @react.component
  let functionReturningReactElement = (~name) => React.string(name)
}

module RenderPropRequiresConversion = {
  @genType @react.component
  let make = (~renderVehicle: {"vehicle": vehicle, "number": int} => React.element) => {
    let car = {name: "Car"}
    renderVehicle({"vehicle": car, "number": 42})
  }
}

module WithChildren = {
  @genType @react.component
  let aComponentWithChildren = (~vehicle, ~children) =>
    <div>
      {React.string("Another Hook " ++ vehicle.name)}
      <div> children </div>
    </div>
}

module DD = {
  @genType @react.component
  let make = (~array as _: Js.TypedArray2.Uint8Array.t, ~name: string) => React.string(name)
}
