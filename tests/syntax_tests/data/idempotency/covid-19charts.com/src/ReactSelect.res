type selectOption<'a> = {
  value: 'a,
  label: string,
}

@module("react-select") @react.component
external make: (
  ~components: _=?,
  ~styles: {..}=?,
  ~controlShouldRenderValue: bool=?,
  ~value: array<selectOption<'a>>=?,
  ~isMulti: bool=?,
  ~name: string=?,
  ~options: array<selectOption<'a>>,
  ~className: string=?,
  ~classNamePrefix: string=?,
  ~menuIsOpen: bool=?,
  ~maxMenuHeight: int=?,
  ~maxHeight: int=?,
  ~placeholder: string=?,
  ~isClearable: bool=?,
  ~onChange: Js.Nullable.t<array<selectOption<'a>>> => unit=?,
  ~noOptionsMessage: unit => option<React.element>,
) => React.element = "default"
