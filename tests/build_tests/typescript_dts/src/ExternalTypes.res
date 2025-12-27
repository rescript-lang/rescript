// Test external types - global TypeScript types
@external("Set")
type mySet<'a>

@external("Map")
type myMap<'k, 'v>

@external("Iterator")
type myIterator<'a>

// Test external types from packages - named import
@external(("mock-named", "Observable"))
type observable<'a>

// External type with ReScript shape validation
@external(("mock-named", "Subscription"))
type subscriptionShape = {unsubscribe: unit => unit, closed: bool}

// External type with UseExternal = true (use external type)
@external(("mock-named", "Subscription", true))
type subscription = {unsubscribe: unit => unit, closed: bool}

// Test default import: import type config from "mock-default"
@external(("mock-default", "default"))
type config

// Test namespace import: import type * as API from "mock-namespace"
@external(("mock-namespace", "*"))
type api

// Use the global external types
let makeSet = (): mySet<string> => %raw(`new Set()`)

let makeMap = (): myMap<string, int> => %raw(`new Map()`)

let getIterator = (_arr: array<'a>): myIterator<'a> => %raw(`_arr[Symbol.iterator]()`)

// Observable bindings - test that external type works with @send
@send external subscribeWithCallback: (observable<'a>, 'a => unit) => subscription = "subscribe"
@send external forEach: (observable<'a>, 'a => unit) => promise<unit> = "forEach"
@send external toPromise: observable<'a> => promise<option<'a>> = "toPromise"

// Create observable (dummy for testing)
let makeObservable = (): observable<string> => %raw(`null`)

// Test using observable methods - this verifies the type flows correctly
let testSubscribe = (obs: observable<string>) => {
  subscribeWithCallback(obs, value => Console.log(value))
}

let testForEach = async (obs: observable<int>) => {
  await forEach(obs, n => Console.log2("value:", n))
}

let testToPromise = async (obs: observable<float>) => {
  let result = await toPromise(obs)
  switch result {
  | Some(v) => Console.log2("got:", v)
  | None => Console.log("none")
  }
}
