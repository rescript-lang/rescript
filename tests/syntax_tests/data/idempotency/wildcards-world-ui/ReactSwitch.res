@module("react-switch") @react.component
external make: (
  ~onChange: bool => unit=?,
  ~checked: bool=?,
  ~onColor: string=?,
  ~onHandleColor: string=?,
  ~offHandleColor: string=?,
  ~uncheckedIcon: bool=?,
  ~checkedIcon: bool=?,
  ~height: int=?,
  ~width: int=?,
  ~handleDiameter: int=?,
  ~className: string=?,
) => React.element = "default"
