module JsComponent = {
  @module("./DatePicker") @react.component
  external make: (
    ~id: string=?,
    ~onChange: Js.Nullable.t<Js.Date.t> => unit,
    ~selected: Js.Date.t=?,
  ) => React.element = "default"
}

@react.component
let make = (~onChange, ~selected=?, ~id=?) =>
  <JsComponent ?id onChange={date => onChange(date->Js.Nullable.toOption)} ?selected />
