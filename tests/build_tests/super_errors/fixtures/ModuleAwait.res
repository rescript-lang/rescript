let f0 = () => {
  module O: module type of Belt.Option = await Belt.Option
  O.forEach
}
