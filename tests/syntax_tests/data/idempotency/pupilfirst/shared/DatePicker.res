module JsComponent = {
  @module("./DatePicker") @react.component
  external make: (
    ~id: string=?,
    ~onChange: nullable<Date.t> => unit,
    ~selected: Date.t=?,
  ) => React.element = "default"
}

@react.component
let make = (~onChange, ~selected=?, ~id=?) =>
  <JsComponent ?id onChange={date => onChange(date->Nullable.toOption)} ?selected />
