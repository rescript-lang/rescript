@module("react-datepicker") @react.component
external make: (
  ~selected: Js.Date.t,
  ~onChange: Js.Date.t => unit,
  ~customInput: React.element,
  ~selectsStart: bool=?,
  ~selectsEnd: bool=?,
  ~startDate: Js.Date.t=?,
  ~endDate: Js.Date.t=?,
  ~minDate: Js.Date.t=?,
) => React.element = "default"
