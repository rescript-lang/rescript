/*** Problematic example of nested promises is safe with the current Promise API. */

let nestedPromise = async (xxx: promise<promise<int>>) => {
  let xx = await xxx

  let _ = xx->Promise.then(x => Promise.resolve(Console.log2("Promise2.then", x)))
  let _ = xx->Promise.catch(x => {
    Console.log2("Promise2.catch_", x)
    Promise.resolve(0)
  })

  // This crashes
  let _ = Promise.then(xx, x => Promise.resolve(Console.log2("Promise.then_", x)))
}

let create = async x => {
  Console.log2("create", x)
  x
}

let xx = create(10)
let xxx = create(xx)
let _ = nestedPromise(xxx)
