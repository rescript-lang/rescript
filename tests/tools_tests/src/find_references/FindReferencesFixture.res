type item = {
  label: string,
}

module Nested = {
  let makeGreeting = (name: string) => "hello " ++ name
}

let makeGreeting = Nested.makeGreeting
let itemToString = (item: item) => item.label
