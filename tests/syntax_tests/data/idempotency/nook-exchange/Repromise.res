let let_ = Promise.flatMap

module Wrap = {
  let let_ = Promise.map
}

module JsExn = {
  exception Unexpected(Js.Promise.error)
  let let_ = (bsPromise, cb) => {
    let promise = bsPromise->Promise.Js.fromBsPromise->Promise.Js.toResult
    Promise.flatMap(promise, result =>
      cb(
        switch result {
        | Ok(result) => result
        | Error(error) =>
          Js.log2("Repromise.JsExn", error)
          throw(Unexpected(error))
        },
      )
    )
  }
}

module Js = {
  let let_ = (bsPromise, cb) => {
    let promise = bsPromise->Promise.Js.fromBsPromise->Promise.Js.toResult
    Promise.flatMap(promise, cb)
  }
}
