module React = {
  type t

  let render = () => Js.log("foo")
}

module Make: () => S = (Config, ()) => {}
module rec Make: (Config, ()) => S = (Config: Config, ()): S => {}
