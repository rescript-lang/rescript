open Belt

module type Config = {
  let configx: Js.Json.t
}
module WebpackConfig: Config = {
  @module external configx: Js.Json.t = "../../../webpack.config.js"
}

module WebpackDevMiddlewareConfig: Config = {
  @module external configx: Js.Json.t = "../../../webpack.middleware.config.js"
}

@module("../../../webpack.middleware.config.js") @val
external configX: unit => Js.Json.t = "configX"

let configX = configX
module U: {
  let configX: unit => Js.Json.t
} = {
  @module("../../../webpack.config.js") @val external configX: unit => Js.Json.t = "configX"
}
@module("List") external hey: unit => unit = "xx"
module A = {
  @module(("reactX", "List")) external ff: unit => unit = "ff"
  @module(("reactX", "List")) external ff2: unit => unit = "ff2"
}
module B = {
  @module(("reactV", "List")) external ff: unit => unit = "ff"
  @module(("reactV", "List")) external ff2: unit => unit = "ff2"
}

let f = () => (A.ff, A.ff2, B.ff, B.ff2)
hey()

(List.length(list{1, 2}), List.length(list{}))->ignore

type t
@module("./local") external ff: unit => t = "ff"
