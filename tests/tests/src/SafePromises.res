/*** Problematic example of nested promises is safe with Js.Promise2 */

let nestedPromise = async (xxx: promise<promise<int>>) => {
  let xx = await xxx

  let _ = xx->Js.Promise2.then(x => Js.Promise.resolve(Console.log2("Promise2.then", x)))
  let _ = xx->Js.Promise2.catch(x => {
    Console.log2("Promise2.catch_", x)
    Js.Promise.resolve(0)
  })

  // This crashes
  let _ = Js.Promise.then_(x => Js.Promise.resolve(Console.log2("Promise.then_", x)), xx)
}

let create = async x => {
  Console.log2("create", x)
  x
}

let xx = create(10)
let xxx = create(xx)
let _ = nestedPromise(xxx)
