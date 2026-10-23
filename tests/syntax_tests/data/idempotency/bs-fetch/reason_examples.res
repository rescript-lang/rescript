let _ = {
  open Promise
  Fetch.fetch("/api/hellos/1")
  ->then_(Fetch.Response.text)
  ->then_(text => print_endline(text)->resolve)
}

let _ = {
  open Promise
  Fetch.fetchWithInit("/api/hello", Fetch.RequestInit.make(~method_=Post, ()))
  ->then_(Fetch.Response.text)
  ->then_(text => print_endline(text)->resolve)
}

let _ = {
  open Promise
  Fetch.fetch("/api/fruit")
  /* assume server returns `["apple", "banana", "pear", ...]` */
  ->then_(Fetch.Response.json)
  ->then_(json => JSON.decodeArray(json)->resolve)
  ->then_(opt => Belt.Option.getExn(opt)->resolve)
  ->then_(items =>
    items->Array.map(item => item->JSON.decodeString->Belt.Option.getExn)->resolve
  )
}

/* makes a post request with the following json payload { hello: "world" } */
let _ = {
  let payload = Dict.make()
  Dict.set(payload, "hello", JSON.string("world"))
  open Promise
  Fetch.fetchWithInit(
    "/api/hello",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(JSON.stringify(JSON.object_(payload))),
      ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
      (),
    ),
  )->then_(Fetch.Response.json)
}
