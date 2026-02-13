type method_ = Post | Get

type body = Json(JSON.t)

type request = {
  method: method_,
  url: string,
  headers: dict<string>,
  body: body,
}

let classify = request =>
  switch request {
  | {
      method: Post,
      url,
      headers: dict{"Authorization": "Token"},
      body: Json(JSON.Object(dict{
        "request": JSON.String("READ"),
        "payload": JSON.Object(dict{
          "list-a": JSON.Object(dict{"last_known_events": JSON.Object(dict{})}),
        }),
      })),
    } if url->String.includes("/todo/events") => 1
  | _ => 0
  }
