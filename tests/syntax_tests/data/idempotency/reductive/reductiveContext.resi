module type Config = {
  type state
  type action
}
module Make: (Config: Config) =>
{
  module Provider: {
    @react.component
    let make: (
      ~children: React.element,
      ~store: Reductive.Store.t<Config.action, Config.state>
    ) => React.element
  }
  let useSelector: (Config.state => 'selectedState) => 'selectedState
  let useDispatch: (unit, Config.action) => unit
  let useStore: unit => Reductive.Store.t<Config.action, Config.state>
}
