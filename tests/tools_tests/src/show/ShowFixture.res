/** Show fixture docs */
type item = {
  label: string,
}

module Nested = {
  /** Nested docs */
  let makeGreeting = (name: string) => "hello " ++ name
}

/** Alias docs */
let makeGreeting = Nested.makeGreeting
