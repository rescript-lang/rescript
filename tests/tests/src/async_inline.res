let willBeInlined = async () => 3

let inlined = willBeInlined()

let wrapSomethingAsync: unit => unit = () => {
  let _ = (
    async _ => {
      let test = await Js.Promise.resolve("Test")
      Console.log(test)
    }
  )(777)
}

external ignorePromise: promise<'a> => unit = "%identity"

let wrapSomethingAsync2 = () =>
  (
    async () => {
      let test = await Js.Promise.resolve("Test")
      Console.log(test)
    }
  )()->ignorePromise

module M: {
  let broken: (unit => promise<'a>) => promise<'a>
} = {
  let doSomethingAsync = async someAsyncFunction => {
    await someAsyncFunction()
  }

  let broken = someAsyncFunction => doSomethingAsync(someAsyncFunction)
}

let broken = async someAsyncFunction => {
  await someAsyncFunction()
}

let broken = someAsyncFunction => broken(someAsyncFunction)

let curriedId = x => x
let curriedIdAsync = async x => x
let uncurriedId = x => x
let uncurriedIdAsync = async x => x

let tci = curriedId(3)
let tcia = curriedIdAsync(3)
let tui = uncurriedId(3)
let tuia = uncurriedIdAsync(3)

let nested1 = () => async y => await y

let nested2 = async () => async y => await y

type callback<'input, 'output> = 'input => 'output

@module("react")
external useCallback: ('input => 'output) => callback<'input, 'output> = "useCallback"

let onSubmit = () =>
  useCallback(async b => {
    await b
  })
