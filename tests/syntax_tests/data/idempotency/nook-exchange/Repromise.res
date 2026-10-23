let let_ = Promise.flatMap

module Wrap = {
  let let_ = Promise.map
}

module JsExn = {
  exception Unexpected(exn)
  let let_ = (bsPromise, cb) => {
    let promise = bsPromise->Promise.thenResolve(value => Ok(value))->Promise.catch(error => Promise.resolve(Error(error)))
    Promise.flatMap(promise, result =>
      cb(
        switch result {
        | Ok(result) => result
        | Error(error) =>
          Console.log2("Repromise.JsExn", error)
          throw(Unexpected(error))
        },
      )
    )
  }
}

module Js = {
  let let_ = (bsPromise, cb) => {
    let promise = bsPromise->Promise.thenResolve(value => Ok(value))->Promise.catch(error => Promise.resolve(Error(error)))
    Promise.flatMap(promise, cb)
  }
}
