@module("@wildcards/react-carousel") @react.component
external make: (
  ~children: React.element,
  ~slidesPerPage: int=?,
  ~centered: bool=?,
  ~value: int=?,
  ~animationSpeed: int=?,
  ~className: string=?,
  ~onChange: int => unit=?,
  ~infinite: bool=?,
  ~autoPlay: int=?,
  ~arrowLeft: React.element=?,
  ~arrowRight: React.element=?,
  ~addArrowClickHandler: bool=?,
  ~arrows: bool=?,
) => React.element = "default"

// ~arrowLeft: React.element=?,
// ~arrowRight: React.element=?,

module Dots = {
  @module("@wildcards/react-carousel") @react.component
  external make: (~onChange: int => unit, ~value: int, ~number: int) => React.element = "Dots"
}
