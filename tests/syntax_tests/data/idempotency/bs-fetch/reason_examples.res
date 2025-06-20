let _ = {
  open Js.Promise
  Fetch.fetch("/api/hellos/1")
  ->then_(Fetch.Response.text)
  ->then_(text => print_endline(text)->resolve)
}

let _ = {
  open Js.Promise
  Fetch.fetchWithInit("/api/hello", Fetch.RequestInit.make(~method_=Post, ()))
  ->then_(Fetch.Response.text)
  ->then_(text => print_endline(text)->resolve)
}

let _ = {
  open Js.Promise
  Fetch.fetch("/api/fruit")
  /* assume server returns `["apple", "banana", "pear", ...]` */
  ->then_(Fetch.Response.json)
  ->then_(json => Js.Json.decodeArray(json)->resolve)
  ->then_(opt => Belt.Option.getExn(opt)->resolve)
  ->then_(items =>
    items->Js.Array.map(item => item->Js.Json.decodeString->Belt.Option.getExn)->resolve
  )
}

/* makes a post request with the following json payload { hello: "world" } */
let _ = {
  let payload = Js.Dict.empty()
  Js.Dict.set(payload, "hello", Js.Json.string("world"))
  open Js.Promise
  Fetch.fetchWithInit(
    "/api/hello",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))),
      ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
      (),
    ),
  )->then_(Fetch.Response.json)
}
