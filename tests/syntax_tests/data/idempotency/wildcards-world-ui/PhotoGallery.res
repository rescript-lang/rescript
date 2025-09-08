type photo = {
  src: string,
  width: int,
  height: int,
}
type selectedPhoto = {
  photo: photo,
  index: int,
  // next: photo,
  // previous: photo,
}

@module("react-photo-gallery") @react.component
external // onClick:
make: (
  ~photos: array<photo>,
  ~targetRowHeight: int,
  ~onClick: (ReactEvent.Mouse.t, selectedPhoto) => unit,
) => React.element = "default"
