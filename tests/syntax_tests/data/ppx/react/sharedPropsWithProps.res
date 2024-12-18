let f = a => Js.Promise.resolve(a + a)

@@jsxConfig({version:4, mode: "classic"})

module V4C1 = {
  type props = sharedProps
  @react.componentWithProps
  let make = (props) => React.string(props.x ++ props.y)
}

module V4C2 = {
  type props = sharedProps
  @react.componentWithProps
  let make = (props: props) => React.string(props.x ++ props.y)
}

module V4C3 = {
  type props<'a> = sharedProps<'a>
  @react.componentWithProps
  let make = ({x, y}: props<_>) => React.string(x ++ y)
}

module V4C4 = {
  type props<'a> = sharedProps<string, 'a>
  @react.componentWithProps
  let make = ({x, y}: props<_>) => React.string(x ++ y)
}

module V4C5 = {
  type props<'a> = {a: 'a}
  @react.componentWithProps
  let make = async ({a}: props<_>) => {
    let a = await f(a)
    <div> {React.int(a)} </div>
  }
}

module V4C6 = {
  type props<'status> = {status: 'status}
  @react.componentWithProps
  let make = async ({status}: props<_>) => {
    switch status {
    | #on => React.string("on")
    | #off => React.string("off")
    }
  }
}

@@jsxConfig({version:4, mode: "automatic"})

module V4A1 = {
  type props = sharedProps
  @react.componentWithProps
  let make = (props) => React.string(props.x ++ props.y)
}

module V4A2 = {
  type props = sharedProps
  @react.componentWithProps
  let make = (props: props) => React.string(props.x ++ props.y)
}

module V4A3 = {
  type props<'a> = sharedProps<'a>
  @react.componentWithProps
  let make = ({x, y}: props<_>) => React.string(x ++ y)
}

module V4A4 = {
  type props<'a> = sharedProps<string, 'a>
  @react.componentWithProps
  let make = ({x, y}: props<_>) => React.string(x ++ y)
}

module V4A5 = {
  type props<'a> = {a: 'a}
  @react.componentWithProps
  let make = async ({a}: props<_>) => {
    let a = await f(a)
    <div> {React.int(a)} </div>
  }
}

module V4A6 = {
  type props<'status> = {status: 'status}
  @react.componentWithProps
  let make = async ({status}: props<_>) => {
    switch status {
    | #on => React.string("on")
    | #off => React.string("off")
    }
  }
}
