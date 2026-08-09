@module("react-datepicker") @react.component
external make: (
  ~selected: Date.t,
  ~onChange: Date.t => unit,
  ~customInput: React.element,
  ~selectsStart: bool=?,
  ~selectsEnd: bool=?,
  ~startDate: Date.t=?,
  ~endDate: Date.t=?,
  ~minDate: Date.t=?,
) => React.element = "default"
